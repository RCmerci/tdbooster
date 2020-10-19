open Std

module Money : sig
  type t [@@deriving sexp_of]

  val create : float -> t

  val buy : t -> float * t

  val left : t -> float

  val sell : t -> float -> t
end = struct
  type t =
    { part1 : float option
    ; part2 : float option
    ; part3 : float option
    ; part4 : float option
    }
  [@@deriving sexp_of]

  let create total =
    { part1 = Some (total /. 2.)
    ; part2 = Some (total /. 2.)
    ; part3 = Some (total *. (0. /. 10.))
    ; part4 = Some (total *. (0. /. 10.))
    }

  let buy t =
    if Option.is_some t.part1 then
      (Option.value_exn t.part1, { t with part1 = None })
    else if Option.is_some t.part2 then
      (Option.value_exn t.part2, { t with part2 = None })
    else if Option.is_some t.part3 then
      (Option.value_exn t.part3, { t with part3 = None })
    else if Option.is_some t.part4 then
      (Option.value_exn t.part4, { t with part4 = None })
    else
      (0., t)

  let left t =
    List.filter_opt [ t.part1; t.part2; t.part3; t.part4 ]
    |> List.sum (module Float) ~f:ident

  let sell t sell_money =
    match (t.part1, t.part2, t.part3, t.part4) with
    | Some _, Some _, Some _, Some _ -> failwith "never buy yet"
    | None, Some _, _, _ -> { t with part1 = Some sell_money }
    | None, None, Some _, _ -> { t with part2 = Some sell_money }
    | None, None, None, Some _ -> { t with part3 = Some sell_money }
    | None, None, None, None -> { t with part4 = Some sell_money }
    | _ -> failwith "unreachable"
end

type txn_phase =
  | Phase0
  | Phase1
  | Phase2
  | Phase3
  | Phase4
[@@deriving sexp_of]

type current_txn =
  { (* code, (date, buy_price, buymoney) *)
    holding_codes : (string * (Date.t * float * float)) list
  ; phase : txn_phase
  ; trend_ever_gt_90 : bool
  ; trend_ever_gt_80 : bool
  ; trend_ever_gt_50 : bool
  ; ever_sold_highestpart : bool
  }
[@@deriving sexp_of]

type state =
  { db : L2.Data.Query.IndustryTrendData.t
  ; db2 : L2.Data.Query.BaseData.t
  ; db3 : L2.Data.Query.DerivedData.t
  ; today : Date.t
  ; txn_history :
      (string, ((Date.t * float) * (Date.t * float) * string) list) Hashtbl.t
  ; current_txn : current_txn
  ; money : Money.t
  ; trading_days : (Date.t, Date.comparator_witness) Set.t
  }

let sexp_of_state state =
  let open Sexp in
  List
    [ List [ Atom "today"; Date.sexp_of_t state.today ]
    ; List
        [ Atom "txn_history"
        ; [%sexp_of:
            ( string
            , ((Date.t * float) * (Date.t * float) * string) list )
            Hashtbl.t] state.txn_history
        ]
    ; List [ Atom "current_txn"; sexp_of_current_txn state.current_txn ]
    ; List [ Atom "money"; Money.sexp_of_t state.money ]
    ]

let rec lastNdays n state =
  if n = 0 then
    state.today
  else
    let last = Date.add_days state.today (-1) in
    if Set.mem state.trading_days last then
      lastNdays (n - 1) { state with today = last }
    else
      lastNdays n { state with today = last }

let lastday = lastNdays 1

let rec nextNdays n state =
  if n = 0 then
    state.today
  else
    let next = Date.add_days state.today 1 in
    if Set.mem state.trading_days next then
      nextNdays (n - 1) { state with today = next }
    else
      nextNdays n { state with today = next }

let nextday = nextNdays 1

let lastNdays_trend n state =
  assert (n <> 0);
  let lastday = lastday state in
  let lastNday = lastNdays n state in
  let trend_lastNdays =
    L2.Data.Query.IndustryTrendData.query state.db ~industries:[ "sum" ]
      ~dwm:`DAY
      ~selector:(L2.Data.Query.Selector.BetweenEE (lastNday, lastday))
  in
  let sum_trend = Map.find_exn trend_lastNdays "sum" in
  assert (Array.length sum_trend = n);
  Array.map sum_trend ~f:(fun e -> e.above_ma20_percent)

let lastday_trend state = (lastNdays_trend 1 state).(0)

let lastday_deriveddata state code =
  let lastday = lastday state in
  let last_deriveddata =
    L2.Data.Query.DerivedData.query state.db3 ~codes:[ code ] ~dwm:`DAY
      ~selector:(L2.Data.Query.Selector.BetweenEE (lastday, lastday))
  in
  let last_deriveddata' = Map.find_exn last_deriveddata code in
  last_deriveddata'.(0)

let today_basedata state code =
  let today_basedata =
    L2.Data.Query.BaseData.query state.db2 ~codes:[ code ] ~dwm:`DAY
      ~selector:(L2.Data.Query.Selector.BetweenEE (state.today, state.today))
  in
  let today_basedata' = Map.find_exn today_basedata code in
  today_basedata'.(0)

let go_on ?phase ?(nextNday = 1) state =
  match phase with
  | None -> { state with today = nextNdays nextNday state }
  | Some phase ->
    { state with
      today = nextNdays nextNday state
    ; current_txn = { state.current_txn with phase }
    }

let close state code =
  let basedata =
    L2.Data.Query.BaseData.query state.db2 ~codes:[ code ] ~dwm:`DAY
      ~selector:(L2.Data.Query.Selector.BetweenEE (state.today, state.today))
  in
  let basedata' = (Map.find_exn basedata code).(0) in
  assert (basedata'.date = state.today);
  basedata'.close

let compute_current_txn current_money current_txn
    (sell_prices : (string, float) Hashtbl.t) =
  let open Option.Monad_infix in
  let txn_money =
    List.map current_txn.holding_codes
      ~f:(fun (code, (_, buy_price, buy_money)) ->
        Hashtbl.find sell_prices code >>= fun sell_price ->
        Some (sell_price /. buy_price *. buy_money))
    |> List.filter_opt
    |> List.sum (module Float) ~f:ident
  in
  Money.left current_money +. txn_money

let add_txn_history state current_txn sell_date sell_price reason =
  List.iter current_txn.holding_codes
    ~f:(fun (code, (buy_date, buy_price, _)) ->
      match Hashtbl.find state.txn_history code with
      | Some l ->
        Hashtbl.set state.txn_history ~key:code
          ~data:(((buy_date, buy_price), (sell_date, sell_price), reason) :: l)
      | None ->
        Hashtbl.set state.txn_history ~key:code
          ~data:[ ((buy_date, buy_price), (sell_date, sell_price), reason) ])

let add_txn_history_one state (one_holding : string * (Date.t * float * float))
    sell_date sell_price =
  add_txn_history state
    { state.current_txn with holding_codes = [ one_holding ] }
    sell_date sell_price

let highest_holding current_txn =
  let sorted =
    List.sort current_txn.holding_codes
      ~compare:(fun (_, (_, buy_price1, _)) (_, (_, buy_price2, _)) ->
        Float.compare buy_price2 buy_price1)
  in
  List.hd sorted

let lowest_holding current_txn =
  let sorted =
    List.sort current_txn.holding_codes
      ~compare:(fun (_, (_, buy_price1, _)) (_, (_, buy_price2, _)) ->
        Float.compare buy_price1 buy_price2)
  in
  List.hd sorted

let remove_highest_holding current_txn =
  let sorted =
    List.sort current_txn.holding_codes
      ~compare:(fun (_, (_, buy_price1, _)) (_, (_, buy_price2, _)) ->
        Float.compare buy_price2 buy_price1)
  in
  Option.value_map (List.tl sorted) ~default:current_txn ~f:(fun e ->
      { current_txn with holding_codes = e })

let eval state test_code =
  let lastday_above_ma20_percent = lastday_trend state in
  match state.current_txn.phase with
  | Phase0 ->
    if lastday_above_ma20_percent < 25. then
      let part, money' = Money.buy state.money in
      let holding =
        (test_code, (state.today, close state test_code, part))
        :: state.current_txn.holding_codes
      in
      let phase = Phase1 in
      { state with
        current_txn =
          { holding_codes = holding
          ; phase
          ; trend_ever_gt_90 = false
          ; trend_ever_gt_80 = false
          ; trend_ever_gt_50 = false
          ; ever_sold_highestpart = false
          }
      ; money = money'
      ; today = nextday state
      }
    else
      { state with today = nextday state }
  | Phase1 ->
    if lastday_above_ma20_percent > 35. then
      { state with
        current_txn = { state.current_txn with phase = Phase4 }
      ; today = nextday state
      }
    else if lastday_above_ma20_percent < 20. then
      let part, money' = Money.buy state.money in
      let holding =
        (test_code, (state.today, close state test_code, part))
        :: state.current_txn.holding_codes
      in
      let phase = Phase2 in
      { state with
        current_txn =
          { holding_codes = holding
          ; phase
          ; trend_ever_gt_90 = false
          ; trend_ever_gt_80 = false
          ; trend_ever_gt_50 = false
          ; ever_sold_highestpart = false
          }
      ; money = money'
      ; today = nextday state
      }
    else
      { state with today = nextday state }
  | Phase2 ->
    if lastday_above_ma20_percent > 35. then
      { state with
        current_txn = { state.current_txn with phase = Phase4 }
      ; today = nextday state
      }
    else if lastday_above_ma20_percent < 15. then
      let part, money' = Money.buy state.money in
      let holding =
        (test_code, (state.today, close state test_code, part))
        :: state.current_txn.holding_codes
      in
      let phase = Phase3 in
      { state with
        current_txn =
          { holding_codes = holding
          ; phase
          ; trend_ever_gt_90 = false
          ; trend_ever_gt_80 = false
          ; trend_ever_gt_50 = false
          ; ever_sold_highestpart = false
          }
      ; money = money'
      ; today = nextday state
      }
    else
      { state with today = nextday state }
  | Phase3 ->
    if lastday_above_ma20_percent > 35. then
      { state with
        current_txn = { state.current_txn with phase = Phase4 }
      ; today = nextday state
      }
    else if lastday_above_ma20_percent < 10. then
      let part, money' = Money.buy state.money in
      let holding =
        match part with
        | 0. -> state.current_txn.holding_codes
        | _ ->
          (test_code, (state.today, close state test_code, part))
          :: state.current_txn.holding_codes
      in
      let phase = Phase3 in
      { state with
        current_txn =
          { holding_codes = holding
          ; phase
          ; trend_ever_gt_90 = false
          ; trend_ever_gt_80 = false
          ; trend_ever_gt_50 = false
          ; ever_sold_highestpart = false
          }
      ; money = money'
      ; today = nextday state
      }
    else
      { state with today = nextday state }
  | Phase4 ->
    let sell_price = close state test_code in
    let money' =
      compute_current_txn state.money state.current_txn
        (Hashtbl.of_alist_exn (module String) [ (test_code, sell_price) ])
    in
    let money = Money.create money' in
    let new_current_txn =
      { holding_codes = []
      ; phase = Phase0
      ; trend_ever_gt_90 = false
      ; trend_ever_gt_80 = false
      ; trend_ever_gt_50 = false
      ; ever_sold_highestpart = false
      }
    in
    if lastday_above_ma20_percent > 90. then
      let _ =
        add_txn_history state state.current_txn state.today sell_price "1"
      in

      let _ = Sexp.to_string (Money.sexp_of_t money) |> Printf.printf "%s\n" in
      { state with current_txn = new_current_txn; today = nextday state; money }
    else if lastday_above_ma20_percent > 80. then
      let current_txn =
        { state.current_txn with
          trend_ever_gt_80 = true
        ; trend_ever_gt_50 = true
        }
      in
      { state with current_txn; today = nextday state }
    else if state.current_txn.trend_ever_gt_80 then
      let _ =
        add_txn_history state state.current_txn state.today sell_price "2"
      in
      let _ = Sexp.to_string (Money.sexp_of_t money) |> Printf.printf "%s\n" in
      { state with current_txn = new_current_txn; today = nextday state; money }
    else if lastday_above_ma20_percent > 50. then
      let current_txn = { state.current_txn with trend_ever_gt_50 = true } in
      { state with current_txn; today = nextday state }
    else if state.current_txn.trend_ever_gt_50 then
      let last2day_above_ma20_percent =
        lastday_trend { state with today = lastday state }
      in
      let last3day_above_ma20_percent =
        lastday_trend
          { state with today = lastday { state with today = lastday state } }
      in
      if
        last2day_above_ma20_percent < last3day_above_ma20_percent
        && lastday_above_ma20_percent < last2day_above_ma20_percent
      then
        let _ =
          add_txn_history state state.current_txn state.today sell_price "3"
        in
        let _ =
          Sexp.to_string (Money.sexp_of_t money) |> Printf.printf "%s\n"
        in
        { state with
          current_txn = new_current_txn
        ; today = nextday state
        ; money
        }
      else
        { state with today = nextday state }
    else
      let _, buy_price, _ =
        List.Assoc.find_exn state.current_txn.holding_codes ~equal:String.equal
          test_code
      in
      if buy_price > sell_price then
        let _ =
          add_txn_history state state.current_txn state.today sell_price "4"
        in
        let _ =
          Sexp.to_string (Money.sexp_of_t money) |> Printf.printf "%s\n"
        in
        { state with
          current_txn = new_current_txn
        ; today = nextday state
        ; money
        }
      else
        { state with today = nextday state }
