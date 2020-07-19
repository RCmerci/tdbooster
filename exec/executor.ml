open Strategy.Cursor
open Core
open Poly    
open Option.Monad_infix

type executorResult =
  | NoAction of {
      name: string;
      next: Data_cursor.t option;(* next date exec should start *)
      logs: Strategy.Log_warning.Log.t;
      warnings: Strategy.Log_warning.Warning.t
    }
  | Transaction of
      {
        name: string;
        buy: Data_cursor.t * float;
        sell: (Data_cursor.t * float) option;
        logs: Strategy.Log_warning.Log.t;
        warnings: Strategy.Log_warning.Warning.t
      }
  | Attention of
      {
        name: string;
        logs: Strategy.Log_warning.Log.t;
        warnings: Strategy.Log_warning.Warning.t
      }

let to_output name (rl:executorResult list):Output.t =
  let translist, attentionlist = List.map rl ~f:(fun r ->
      match r with
      | NoAction _ -> (None, None)
      | Transaction {buy;sell;logs;warnings;_} ->
        (Some ({i={date=(Data_cursor.datestring(fst buy)); price=Float.round_decimal ~decimal_digits:2 (snd buy); info=[]};
                o=Option.map sell
                    ~f:(fun sell -> ({date=(Data_cursor.datestring(fst sell)); price=Float.round_decimal ~decimal_digits:2 (snd sell); info=[]}:Output.transinfo));
                logs=Strategy.Log_warning.Log.to_string_list logs;
                warnings=Strategy.Log_warning.Warning.to_string_list warnings;
               }:Output.transaction),None)
      | Attention {logs; warnings; _} ->
        ( None , Some ({logs=Strategy.Log_warning.Log.to_string_list logs;
                        warnings=Strategy.Log_warning.Warning.to_string_list warnings}:Output.attention))
    ) |> List.unzip in

  [{name; trans=List.filter_opt translist; attentions=List.filter_opt attentionlist}]


let atleast_next_day atleast_next_this_c d2 =
  let atleast_next_this_date = Data_cursor.date atleast_next_this_c in
  let d2' = Data_cursor.date d2 in
  if atleast_next_this_date >= d2' then
    Date.add_days atleast_next_this_date 1
  else d2'


module Make (St : Strategy.Type.Strategy) = struct
  type t = {month_k: Data_cursor.t; week_k: Data_cursor.t; day_k: Data_cursor.t}

  let create raw_data =
    Deriving.Unify.unify_month raw_data
    >>= fun month_k_data ->
    Deriving.Unify.unify_week raw_data
    >>= fun week_k_data ->
    Deriving.Unify.unify_day raw_data
    >>= fun day_k_data ->
    Data_cursor.create month_k_data
    >>= fun month_k ->
    Data_cursor.create week_k_data
    >>= fun week_k ->
    Data_cursor.create day_k_data >>= fun day_k -> Some {month_k; week_k; day_k}

  let exec name t =
    let open Strategy.Log_warning in
    let rec aux ?(logslist=[]) ?(warningslist=[]) tp f t ctx uplevel_ctx =
      let logs, warnings, v = Strategy.Log_warning.LogAndWarnWriter.eval (f t ctx uplevel_ctx) in
      match v with
      | Strategy.Type.Buy t' ->
        `Buy (t',
              Strategy.Log_warning.Log.concat (logs::logslist),
              Strategy.Log_warning.Warning.concat (warnings::warningslist))
      | Strategy.Type.Buy_skip_to (ctx', t') ->
        let t'' = (Data_cursor.goto_date t' (atleast_next_day t t')) in
        (match (tp, t'') with
         | (`Month, _) -> `Quit(t'',
                                        Strategy.Log_warning.Log.concat(logs::logslist),
                                        Strategy.Log_warning.Warning.concat (warnings::warningslist))
         | (_, Some t''') ->
           aux tp f t''' ctx' uplevel_ctx ~logslist:(logs::logslist) ~warningslist:(warnings::warningslist)
         | (_, None) -> `NotFinished (None,
                                 Strategy.Log_warning.Log.concat (logs::logslist),
                                 Strategy.Log_warning.Warning.concat (warnings::warningslist)))
      | Strategy.Type.Buy_continue ctx' ->
        let t', n = Data_cursor.move t 1 in
        if n = 0 then `NotFinished (None,
                                    Strategy.Log_warning.Log.concat (logs::logslist),
                                    Strategy.Log_warning.Warning.concat (warnings::warningslist))
        else
          (match tp with
           | `Month -> `Quit(Some t',
                             Strategy.Log_warning.Log.concat (logs::logslist),
                             Strategy.Log_warning.Warning.concat (warnings::warningslist))
           | _ -> aux tp f t' ctx' uplevel_ctx ~logslist:(logs::logslist) ~warningslist:(warnings::warningslist))
      | Strategy.Type.Buy_quit quit_c ->
        `Quit (Some quit_c,
               Strategy.Log_warning.Log.concat(logs::logslist),
               Strategy.Log_warning.Warning.concat (warnings::warningslist))
    in
    let month_aux = aux `Month St.month_k_buy in
    let week_aux = aux `Week St.week_k_buy in
    let day_aux = aux `Day St.day_k_buy in
    match month_aux t.month_k None () with
    | `Quit (quit_c, logs, warnings) ->
      NoAction {next=quit_c;logs;warnings;name}
    | `NotFinished (_, logs,warnings) -> (* month_buy's `NotFinished is nothing *)
      NoAction {next=None;logs;warnings;name}
    | `Buy ((result_month_c, _, month_to_week_ctx), month_logs, month_warnings) ->
      (* 1个月结束了才有月k,所以后面的计算需要在这个月之后 *)
      let next_month,n = Data_cursor.move result_month_c 1 in
      if n = 0 then NoAction{next=(Some next_month); logs=month_logs;warnings=month_warnings;name} else
        Option.value_map ~default:(NoAction {next=(Some next_month); logs=month_logs;warnings=month_warnings;name})
          (Data_cursor.goto_date t.week_k (Data_cursor.date next_month))
          ~f:(fun start_week_c ->
              match week_aux start_week_c None month_to_week_ctx with
              | `Quit (quit_c,week_logs,week_warnings) ->
                NoAction {next=quit_c;
                          logs=Log.concat [week_logs;month_logs ];
                          warnings=Warning.concat [week_warnings;month_warnings];
                          name}
              | `NotFinished (_, week_logs,week_warnings) ->
                Attention {name;logs=Log.concat[week_logs;month_logs];warnings=Warning.concat [week_warnings;month_warnings]}
              | `Buy ((result_week_c, _, week_to_day_ctx), week_logs,week_warnings) ->
                Option.value_map ~default:(NoAction {next=(Some result_week_c);
                                                     logs=Log.concat [week_logs;month_logs];
                                                     warnings=Warning.concat [week_warnings;month_warnings];
                                                     name})
                  (Data_cursor.goto_date t.day_k
                     (Data_cursor.date result_week_c))
                  ~f:(fun start_day_c ->
                      match day_aux start_day_c None week_to_day_ctx with
                      | `Quit (quit_c,day_logs,day_warnings) ->
                        NoAction {next=quit_c;
                                  logs=Log.concat [day_logs;week_logs;month_logs];
                                  warnings=Warning.concat [day_warnings;week_warnings;month_warnings];
                                  name}
                      | `NotFinished (_, day_logs, day_warnings) ->
                        Attention{name;
                                  logs=Log.concat [day_logs;week_logs;month_logs];
                                  warnings=Warning.concat [day_warnings;week_warnings;month_warnings]}
                      | `Buy ((buy_c, buy_price', day_to_sell_ctx), day_logs,day_warnings) ->
                        let buy_price = Option.value_exn buy_price' in
                        let logs', warnings', sell = Strategy.Log_warning.LogAndWarnWriter.eval
                            (St.sell ~buy_c ~buy_price day_to_sell_ctx
                               (Data_cursor.to_k_list t.day_k)
                               (Data_cursor.to_k_list t.week_k)
                               (Data_cursor.to_k_list t.month_k)) in
                        Transaction
                          { name;
                            logs= Log.concat [logs';day_logs;week_logs; month_logs]
                          ;warnings= Warning.concat [warnings';day_warnings;week_warnings;month_warnings];
                            buy= (buy_c, buy_price)
                          ; sell } ) )

  let rec exec_aux name t r nextf =
    let result = exec name t in
    match result with
    | NoAction v ->
      (match v.next with
       | None -> r
       | Some next_c ->
         (match nextf t (Data_cursor.date next_c) >>| fun t' ->
            exec_aux name t' r nextf with
         | None ->
           r
         | Some v ->
           v ))
    | Transaction buysell when Option.is_none buysell.sell ->
      result :: r
    | Transaction buysell -> (
        let sell, _ = Option.value_exn buysell.sell in
        let next_startdate = atleast_next_day (fst buysell.buy) sell in
        match nextf t next_startdate with
        | Some {month_k; week_k; day_k} ->
          (exec_aux name {month_k; week_k; day_k} (result :: r) nextf)
        | None -> result :: r)
    | Attention _ -> (
        result::r
      )

  let exec_sequence name t =
    let goto_next_c t next_c =
      let d = next_c in
      let d'= if (Data_cursor.date t.day_k) >= d then Date.add_days (Data_cursor.date t.day_k) 1 else d in
      Data_cursor.goto_date t.month_k d' ~hint:`Month
      >>= fun month_k ->
      Data_cursor.goto_date t.week_k d' ~hint:`Week
      >>= fun week_k ->
      Data_cursor.goto_date t.day_k d'
      >>= fun day_k -> Some {month_k; week_k; day_k}
    in
    exec_aux name t [] goto_next_c

  let exec_every_month name t =
    let next_month t _ =
      let month_first_day =
        let monthdate = Data_cursor.date t.month_k in
        Date.(add_days
                (add_months monthdate 1)
                (1 - (day monthdate)))
      in
      Data_cursor.goto_date t.month_k month_first_day ~hint:`Month >>=
      fun month_k ->
      Data_cursor.goto_date t.week_k month_first_day ~hint:`Week >>=
      fun week_k ->
      Data_cursor.goto_date t.day_k month_first_day >>=
      fun day_k -> Some {month_k;week_k;day_k}
    in
    exec_aux name t [] next_month
end


