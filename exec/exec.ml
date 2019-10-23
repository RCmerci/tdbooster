open Strategy.Cursor
open Core
open Option.Monad_infix

type executorResult =
  | NoAction
  | Transaction of
      { buy: Data_cursor.t * float
      ; sell: (Data_cursor.t * float) option }

type statistics =
  { buy: Date.t option
  ; sell: Date.t option
  ; duration: int
  ; price_diff: float
  ; buy_price: float
  ; sell_price: float option }
[@@deriving show]

let stat r =
  match r with
  | NoAction ->
      { duration= 0
      ; price_diff= 0.
      ; buy= None
      ; sell= None
      ; buy_price= 0.
      ; sell_price= None }
  | Transaction tr ->
      let duration =
        match tr.sell with
        | None ->
            -1
        | Some (sellc, _) ->
            Date.diff (Data_cursor.date sellc) (Data_cursor.date (fst tr.buy))
      in
      let price_diff =
        match tr.sell with
        | None ->
            -1.
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
      ; sell_price }

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
          Some t'
      | Strategy.Type.Buy_skip_to (ctx', t') ->
          aux f t' ctx'
      | Strategy.Type.Buy_continue ctx' ->
          let t', n = Data_cursor.move t 1 in
          if n = 0 then None else aux f t' ctx'
    in
    let month_aux = aux St.month_k_buy in
    let week_aux = aux St.week_k_buy in
    let day_aux = aux St.day_k_buy in
    let result =
      month_aux t.month_k None
      >>= fun (result_month_c, _) ->
      Data_cursor.goto_date t.week_k
        (Deriving.Type.Derived_data.date (Data_cursor.current result_month_c))
      >>= fun start_week_c ->
      week_aux start_week_c None
      >>= fun (result_week_c, _) ->
      Data_cursor.goto_date t.day_k
        (Deriving.Type.Derived_data.date (Data_cursor.current result_week_c))
      >>= fun start_day_c ->
      day_aux start_day_c None
      >>= fun (buy_c, buy_price) ->
      Some
        ( buy_price
        , buy_c
        , St.sell ~buy_c
            (Data_cursor.to_k_list t.day_k)
            (Data_cursor.to_k_list t.week_k)
            (Data_cursor.to_k_list t.month_k) )
    in
    match result with
    | None ->
        NoAction
    | Some (buy_price, buy, sell) ->
        Transaction {buy= (buy, Option.value_exn buy_price); sell}

  let exec_sequence t =
    let rec aux t r =
      let result = exec t in
      match result with
      | NoAction ->
          r
      | Transaction buysell when Option.is_none buysell.sell ->
          result :: r
      | Transaction buysell -> (
          let sell, _ = Option.value_exn buysell.sell in
          let next_startdate =
            Deriving.Type.Derived_data.date (Data_cursor.current sell)
          in
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
