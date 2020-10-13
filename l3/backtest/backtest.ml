open Std

module Money : sig
  type t [@@deriving sexp_of]

  val create : float -> t

  val buy : t -> float * t

  val left : t -> float
end = struct
  type t =
    { part1 : float option
    ; part2 : float option
    ; part3 : float option
    ; part4 : float option
    ; total : float
    }
  [@@deriving sexp_of]

  let create total =
    { part1 = Some (total /. 4.)
    ; part2 = Some (total /. 4.)
    ; part3 = Some (total /. 4.)
    ; part4 = Some (total /. 4.)
    ; total
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
  }
[@@deriving sexp_of]

type state =
  { db : L2.Data.Query.IndustryTrendData.t
  ; db2 : L2.Data.Query.BaseData.t
  ; today : Date.t
  ; txn_history : (string, ((Date.t * float) * (Date.t * float)) list) Hashtbl.t
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
            (string, ((Date.t * float) * (Date.t * float)) list) Hashtbl.t]
            state.txn_history
        ]
    ; List [ Atom "current_txn"; sexp_of_current_txn state.current_txn ]
    ; List [ Atom "money"; Money.sexp_of_t state.money ]
    ]

let rec lastday_trend ?(count = 30) state =
  if count = 0 then failwithf "state.today:%s" (Date.to_string state.today) ();
  let lastday = Date.add_days state.today (-1) in
  let trend_lastday =
    L2.Data.Query.IndustryTrendData.query state.db ~industries:[ "sum" ]
      ~dwm:`DAY
      ~selector:(L2.Data.Query.Selector.BetweenEE (lastday, lastday))
  in
  let sum_trend_today = Map.find_exn trend_lastday "sum" in
  if Array.length sum_trend_today = 0 then
    lastday_trend ~count:(count - 1) { state with today = lastday }
  else
    sum_trend_today.(0).above_ma20_percent

let rec nextday state =
  let next = Date.add_days state.today 1 in
  if Set.mem state.trading_days next then
    next
  else
    nextday { state with today = next }

let rec lastday state =
  let last = Date.add_days state.today (-1) in
  if Set.mem state.trading_days last then
    last
  else
    lastday { state with today = last }

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

let add_txn_history state current_txn sell_date sell_price =
  List.iter current_txn.holding_codes
    ~f:(fun (code, (buy_date, buy_price, _)) ->
      match Hashtbl.find state.txn_history code with
      | Some l ->
        Hashtbl.set state.txn_history ~key:code
          ~data:(((buy_date, buy_price), (sell_date, sell_price)) :: l)
      | None ->
        Hashtbl.set state.txn_history ~key:code
          ~data:[ ((buy_date, buy_price), (sell_date, sell_price)) ])

(** aaa - phase0 & sum trend < 25: ->phase1 - phase1 & sum trend < 20: -> phase2
    phase1 & sum trend > 35: -> phase4 phase2 & (sum trend < 15 | mean(4day sum
    trend) < 20) -> phase3 phase2 & sum trend > 35: -> phase4 phase3 & sum trend
    < 10: -> phase4 phase3 & sum trend > 35: -> phase4 phase4: sum trend > 90 ->
    sell all sum trend(ever) > 80 & last < 80 -> sell all sum trend(ever) > 50 &
    trend 连续2次下跌 -> sell all sum 跌破成本 -> sell all *)

let test_code = "sh.601318"

let eval state =
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
      }
    in
    if lastday_above_ma20_percent > 90. then
      let _ = add_txn_history state state.current_txn state.today sell_price in
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
      let _ = add_txn_history state state.current_txn state.today sell_price in
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
          add_txn_history state state.current_txn state.today sell_price
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
          add_txn_history state state.current_txn state.today sell_price
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

let rec run_aux state finish_day =
  let nextstate = eval state in
  if nextstate.today > finish_day then
    nextstate
  else
    run_aux nextstate finish_day

let run config_dir init_day finish_day =
  let basedata_db = L2.Data.Query.BaseData.create config_dir in
  let industrytrend_db = L2.Data.Query.IndustryTrendData.create config_dir in
  let basedatamap =
    L2.Data.Query.BaseData.query basedata_db ~codes:[ L2.Data.Const.zz800 ]
      ~dwm:`DAY ~selector:L2.Data.Query.Selector.ALL
  in
  let trading_days =
    Map.find_exn basedatamap L2.Data.Const.zz800
    |> Array.map ~f:(fun (e : L2.Data.Type.base_data_elem) -> e.date)
    |> Set.of_array (module Date)
  in
  let state =
    { db = industrytrend_db
    ; db2 = basedata_db
    ; today = init_day
    ; txn_history = Hashtbl.create (module String)
    ; current_txn =
        { holding_codes = []
        ; phase = Phase0
        ; trend_ever_gt_90 = false
        ; trend_ever_gt_80 = false
        ; trend_ever_gt_50 = false
        }
    ; money = Money.create 10000.
    ; trading_days
    }
  in
  run_aux state finish_day
