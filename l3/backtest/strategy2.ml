open Std
open Util

let eval (state : state) custom_codes =
  let lastday = lastday state in
  let lastday_above_ma20_percent = lastday_trend state in
  match state.current_txn.phase with
  | Phase0 ->
    let filtered_codes =
      L2.Data.Op.(
        search state.op_day ~codes:custom_codes
          Condition.(LE (DateBetweenEE (lastday, lastday), Rsi6 20.)))
    in
    let filtered_codes2 =
      L2.Data.Op.(
        search state.op_day ~codes:filtered_codes
          Condition.(GT (DateBetweenEE (lastday, lastday), Rel20 0.)))
    in
    let filtered_codes3 =
      L2.Data.Op.(
        search state.op_day ~codes:filtered_codes2
          Condition.(GT (DateBetweenEE (lastday, lastday), Rel120 0.)))
    in
    if List.length filtered_codes3 = 0 then
      go_on state
    else
      let test_code = List.hd_exn filtered_codes2 in
      let today_basedata = today_basedata state test_code in
      let today_up_less_3 = today_basedata.percent_change < 0.03 in
      if lastday_above_ma20_percent < 25. && today_up_less_3 then
        let part, money' = Money.buy state.money in
        let transactions =
          (test_code, (state.today, close state test_code, part))
          :: state.current_txn.transactions
        in
        let phase = Phase1 in
        { state with
          current_txn =
            { holding_code = Some test_code
            ; transactions
            ; phase
            ; trend_ever_gt_90 = false
            ; trend_ever_gt_80 = false
            ; trend_ever_gt_50 = false
            ; ever_sold_highestpart = false
            }
        ; money = money'
        ; today = nextNdays 2 state
        }
      else
        go_on state
  | Phase1 ->
    let test_code = Option.value_exn state.current_txn.holding_code in
    let last_rsi6 = (lastday_deriveddata state test_code).rsi6 in
    let today_basedata = today_basedata state test_code in
    let today_up_less_3 = today_basedata.percent_change < 0.03 in
    if lastday_above_ma20_percent > 35. then
      go_on ~phase:Phase4 ~nextNday:5 state
    else if
      lastday_above_ma20_percent < 20. && last_rsi6 < 25. && today_up_less_3
    then
      let part, money' = Money.buy state.money in
      let transactions =
        (test_code, (state.today, close state test_code, part))
        :: state.current_txn.transactions
      in
      let phase = Phase2 in
      { state with
        current_txn =
          { holding_code = Some test_code
          ; transactions
          ; phase
          ; trend_ever_gt_90 = false
          ; trend_ever_gt_80 = false
          ; trend_ever_gt_50 = false
          ; ever_sold_highestpart = false
          }
      ; money = money'
      ; today = nextNdays 1 state
      }
    else
      go_on state
  | Phase2 ->
    let test_code = Option.value_exn state.current_txn.holding_code in
    let last_rsi6 = (lastday_deriveddata state test_code).rsi6 in
    let today_basedata = today_basedata state test_code in
    let today_up_less_3 = today_basedata.percent_change < 0.03 in
    if lastday_above_ma20_percent > 35. then
      go_on ~phase:Phase4 ~nextNday:5 state
    else if
      lastday_above_ma20_percent < 15. && last_rsi6 < 25. && today_up_less_3
    then
      let part, money' = Money.buy state.money in
      let transactions =
        (test_code, (state.today, close state test_code, part))
        :: state.current_txn.transactions
      in
      let phase = Phase3 in
      { state with
        current_txn =
          { holding_code = Some test_code
          ; transactions
          ; phase
          ; trend_ever_gt_90 = false
          ; trend_ever_gt_80 = false
          ; trend_ever_gt_50 = false
          ; ever_sold_highestpart = false
          }
      ; money = money'
      ; today = nextNdays 1 state
      }
    else
      go_on state
  | Phase3 ->
    let test_code = Option.value_exn state.current_txn.holding_code in
    let last_rsi6 = (lastday_deriveddata state test_code).rsi6 in
    let today_basedata = today_basedata state test_code in
    let today_up_less_3 = today_basedata.percent_change < 0.03 in
    if lastday_above_ma20_percent > 35. then
      go_on ~phase:Phase4 ~nextNday:5 state
    else if
      lastday_above_ma20_percent < 10. && last_rsi6 < 25. && today_up_less_3
    then
      let part, money' = Money.buy state.money in
      let transactions =
        (test_code, (state.today, close state test_code, part))
        :: state.current_txn.transactions
      in
      let phase = Phase4 in
      { state with
        current_txn =
          { holding_code = Some test_code
          ; transactions
          ; phase
          ; trend_ever_gt_90 = false
          ; trend_ever_gt_80 = false
          ; trend_ever_gt_50 = false
          ; ever_sold_highestpart = false
          }
      ; money = money'
      ; today = nextNdays 1 state
      }
    else
      go_on state
  | Phase4 ->
    let test_code = Option.value_exn state.current_txn.holding_code in
    let sell_price = close state test_code in
    let money' =
      compute_current_txn state.money state.current_txn
        (Hashtbl.of_alist_exn (module String) [ (test_code, sell_price) ])
    in
    let money = Money.create money' in
    let new_current_txn =
      { holding_code = None
      ; transactions = []
      ; phase = Phase0
      ; trend_ever_gt_90 = false
      ; trend_ever_gt_80 = false
      ; trend_ever_gt_50 = false
      ; ever_sold_highestpart = false
      }
    in
    let _, (_, lowest_buy_price, _) =
      Option.value_exn (lowest_holding state.current_txn)
    in
    if (sell_price -. lowest_buy_price) /. lowest_buy_price > 0.1 then
      let _ =
        add_txn_history state state.current_txn state.today sell_price "1"
      in
      { state with current_txn = new_current_txn; today = nextday state; money }
    else
      let _, buy_price, _ =
        List.Assoc.find_exn state.current_txn.transactions ~equal:String.equal
          test_code
      in
      if buy_price *. 0.95 > sell_price then
        let _ =
          add_txn_history state state.current_txn state.today sell_price "6"
        in
        { state with
          current_txn = new_current_txn
        ; today = nextday state
        ; money
        }
      else
        go_on state

(* | Phase4 ->
 *   let last5day_above_ma20_percent = lastNdays_trend 5 state in
 *   let last2day_above_ma20_percent =
 *     Array.sub last5day_above_ma20_percent ~pos:3 ~len:2
 *   in
 *   let sell_price = close state test_code in
 *   let money' =
 *     compute_current_txn state.money state.current_txn
 *       (Hashtbl.of_alist_exn (module String) [ (test_code, sell_price) ])
 *   in
 *   let money = Money.create money' in
 *   let new_current_txn =
 *     { holding_codes = []
 *     ; phase = Phase0
 *     ; trend_ever_gt_90 = false
 *     ; trend_ever_gt_80 = false
 *     ; trend_ever_gt_50 = false
 *     ; ever_sold_highestpart = false
 *     }
 *   in
 *   if Array.for_all last2day_above_ma20_percent ~f:(fun e -> e > 90.) then
 *     let _ =
 *       add_txn_history state state.current_txn state.today sell_price "1"
 *     in
 *     { state with current_txn = new_current_txn; today = nextday state; money }
 *   else if lastday_above_ma20_percent > 90. then
 *     { state with
 *       current_txn =
 *         { state.current_txn with
 *           trend_ever_gt_90 = true
 *         ; trend_ever_gt_80 = true
 *         ; trend_ever_gt_50 = true
 *         }
 *     ; today = nextday state
 *     }
 *   else if Array.for_all last5day_above_ma20_percent ~f:(fun e -> e > 80.) then
 *     let _ =
 *       add_txn_history state state.current_txn state.today sell_price "2"
 *     in
 *     { state with current_txn = new_current_txn; today = nextday state; money }
 *   else if lastday_above_ma20_percent > 80. then
 *     { state with
 *       current_txn =
 *         { state.current_txn with
 *           trend_ever_gt_90 = false
 *         ; trend_ever_gt_80 = true
 *         ; trend_ever_gt_50 = true
 *         }
 *     ; today = nextday state
 *     }
 *   else if lastday_above_ma20_percent > 70. then
 *     if state.current_txn.trend_ever_gt_90 then
 *       let _ =
 *         add_txn_history state state.current_txn state.today sell_price "3"
 *       in
 *       { state with
 *         current_txn = new_current_txn
 *       ; today = nextday state
 *       ; money
 *       }
 *     else
 *       { state with
 *         current_txn =
 *           { state.current_txn with
 *             trend_ever_gt_90 = false
 *           ; trend_ever_gt_80 = false
 *           ; trend_ever_gt_50 = true
 *           }
 *       ; today = nextday state
 *       }
 *   else if lastday_above_ma20_percent > 50. then
 *     { state with
 *       current_txn =
 *         { state.current_txn with
 *           trend_ever_gt_90 = false
 *         ; trend_ever_gt_80 = false
 *         ; trend_ever_gt_50 = true
 *         }
 *     ; today = nextday state
 *     }
 *   (\* match highest_holding state.current_txn with
 *    * | None ->
 *    *   { state with
 *    *     current_txn =
 *    *       { state.current_txn with
 *    *         trend_ever_gt_90 = false
 *    *       ; trend_ever_gt_80 = false
 *    *       ; trend_ever_gt_50 = true
 *    *       }
 *    *   ; today = nextday state
 *    *   }
 *    * | Some v ->
 *    *   let _, (_, buy_price, buy_money) = v in
 *    *   if
 *    *     (not state.current_txn.ever_sold_highestpart)
 *    *     && sell_price > buy_price
 *    *   then
 *    *     let money' =
 *    *       Money.sell state.money (sell_price /. buy_price *. buy_money)
 *    *     in
 *    *     let current_txn = remove_highest_holding state.current_txn in
 *    *     let phase =
 *    *       if List.length current_txn.holding_codes = 0 then
 *    *         Phase0
 *    *       else
 *    *         current_txn.phase
 *    *     in
 *    *     let current_txn' =
 *    *       { current_txn with phase; ever_sold_highestpart = true }
 *    *     in
 *    *     let _ = add_txn_history_one state v state.today sell_price "4" in
 *    *     { state with
 *    *       current_txn = current_txn'
 *    *     ; money = money'
 *    *     ; today = nextday state
 *    *     }
 *    *   else
 *    *     { state with
 *    *       current_txn =
 *    *         { state.current_txn with
 *    *           trend_ever_gt_90 = false
 *    *         ; trend_ever_gt_80 = false
 *    *         ; trend_ever_gt_50 = true
 *    *         }
 *    *     ; today = nextday state
 *    *     } *\)
 *   else if
 *     state.current_txn.trend_ever_gt_50
 *     && last2day_above_ma20_percent.(0) > last2day_above_ma20_percent.(1)
 *   then
 *     let _ =
 *       add_txn_history state state.current_txn state.today sell_price "5"
 *     in
 *     { state with current_txn = new_current_txn; today = nextday state; money }
 *   else
 *     let _, buy_price, _ =
 *       List.Assoc.find_exn state.current_txn.holding_codes ~equal:String.equal
 *         test_code
 *     in
 *     if buy_price > sell_price then
 *       let _ =
 *         add_txn_history state state.current_txn state.today sell_price "6"
 *       in
 *       { state with
 *         current_txn = new_current_txn
 *       ; today = nextday state
 *       ; money
 *       }
 *     else
 *       go_on state *)
