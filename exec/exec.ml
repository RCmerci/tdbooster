open Strategy.Cursor
open Core
open Option.Monad_infix

type executorResult =
  | NoAction of Data_cursor.t option (* next date exec should start *)
  | Transaction of
      { buy: Data_cursor.t * float
      ; sell: (Data_cursor.t * float) option }

type statistics =
  { buy: Date.t option
  ; sell: Date.t option
  ; duration: int
  ; price_diff: float
  ; buy_price: float
  ; sell_price: float option
  ; percent: float option (* 不计算股价<0的情况 *) }
[@@deriving show]

let stat r =
  match r with
  | NoAction _ ->
      { duration= 0
      ; price_diff= 0.
      ; buy= None
      ; sell= None
      ; buy_price= 0.
      ; sell_price= None
      ; percent= None }
  | Transaction tr ->
      let duration =
        match tr.sell with
        | None ->
            0
        | Some (sellc, _) ->
            Date.diff (Data_cursor.date sellc) (Data_cursor.date (fst tr.buy))
      in
      let price_diff =
        match tr.sell with
        | None ->
            0.
        | Some (_, sellp) ->
            sellp -. snd tr.buy
      in
      let sell =
        Option.value_map tr.sell ~default:None ~f:(fun (c, _) ->
            Some (Data_cursor.date c) )
      in
      let sell_price = tr.sell >>| snd in
      { buy= Some (Data_cursor.date (fst tr.buy))
      ; sell
      ; duration
      ; price_diff
      ; buy_price= snd tr.buy
      ; sell_price
      ; percent=
          (if snd tr.buy < 0. then None else Some (price_diff /. snd tr.buy))
      }

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

  let exec t =
    let rec aux f t ctx =
      match f t ctx with
      | Strategy.Type.Buy t' ->
          First t'
      | Strategy.Type.Buy_skip_to (ctx', t') ->
          aux f t' ctx'
      | Strategy.Type.Buy_continue ctx' ->
          let t', n = Data_cursor.move t 1 in
          if n = 0 then Second None else aux f t' ctx'
      | Strategy.Type.Buy_quit quit_c ->
          Second (Some quit_c)
    in
    let month_aux = aux St.month_k_buy in
    let week_aux = aux St.week_k_buy in
    let day_aux = aux St.day_k_buy in
    match month_aux t.month_k None with
    | Second quit_c ->
        NoAction quit_c
    | First (result_month_c, _) ->
        Option.value_map ~default:(NoAction (Some result_month_c))
          (Data_cursor.goto_date t.week_k (Data_cursor.date result_month_c))
          ~f:(fun start_week_c ->
            match week_aux start_week_c None with
            | Second quit_c ->
                NoAction quit_c
            | First (result_week_c, _) ->
                Option.value_map ~default:(NoAction (Some result_week_c))
                  (Data_cursor.goto_date t.day_k
                     (Data_cursor.date result_week_c))
                  ~f:(fun start_day_c ->
                    match day_aux start_day_c None with
                    | Second quit_c ->
                        NoAction quit_c
                    | First (buy_c, buy_price') ->
                        let buy_price = Option.value_exn buy_price' in
                        Transaction
                          { buy= (buy_c, buy_price)
                          ; sell=
                              St.sell ~buy_c ~buy_price
                                (Data_cursor.to_k_list t.day_k)
                                (Data_cursor.to_k_list t.week_k)
                                (Data_cursor.to_k_list t.month_k) } ) )

  let exec_sequence t =
    let goto_next_c t next_c =
      let d = Data_cursor.date next_c in
      Data_cursor.goto_date t.month_k d
      >>= fun month_k ->
      Data_cursor.goto_date t.week_k d
      >>= fun week_k ->
      Data_cursor.goto_date t.day_k d
      >>= fun day_k -> Some {month_k; week_k; day_k}
    in
    let rec aux t r =
      let result = exec t in
      match result with
      | NoAction None ->
          r
      | NoAction (Some next_c) -> (
        match goto_next_c t next_c >>| fun t' -> aux t' r with
        | None ->
            r
        | Some v ->
            v )
      | Transaction buysell when Option.is_none buysell.sell ->
          result :: r
      | Transaction buysell -> (
          let sell, _ = Option.value_exn buysell.sell in
          let next_startdate = Data_cursor.date sell in
          let r' =
            Data_cursor.goto_date t.month_k next_startdate
            >>= fun month_k ->
            Data_cursor.goto_date t.week_k next_startdate
            >>= fun week_k ->
            Data_cursor.goto_date t.day_k next_startdate
            >>= fun day_k -> Some (aux {month_k; week_k; day_k} (result :: r))
          in
          match r' with None -> result :: r | Some v -> v )
    in
    aux t [] |> List.rev
end

let%test_module _ =
  ( module struct
    let datal =
      Loader.From_tonghuashun_txt.read_from_string_lines
        (String.split_lines Testdata.Data.data)

    module E = Make (Strategy.V1)

    let t = Option.value_exn (E.create datal)

    let%test "test-exec_sequence" =
      Debug.eprint
        (List.to_string (E.exec_sequence t) ~f:(fun e ->
             show_statistics (stat e) )) ;
      false
  end )
