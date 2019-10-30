open Core
open Type
open Cursor
module O = Option.Monad_infix
open Log_warning.LogAndWarnWriter

type ctx = Data_cursor.t

(* 1. (ma20_n+1 > ma_n)
 * 2. ema20斜率 >0
 * 3. month_k-ma20 < 5% * month_k
 * 4. last_macd*1.1 < current_macd
 * 5. ma60 > monthk*0.9
 *)
let month_k_buy c _ : ctx buy_action Log_warning.LogAndWarnWriter.m =
  let left = Data_cursor.left c 3 in
  if List.length left < 3 then return (Buy_continue None)
  else
    let current = Data_cursor.current c in
    (* 1 *)
    let ma_slope_gt_10 = List.map2_exn (List.tl_exn left) (List.sub left ~pos:0 ~len:(List.length left - 1))
        ~f:(fun e1 e2 -> e1.ma20 -. e2.ma20 ) |> List.for_all ~f:(fun e -> e > 0.)
    in
    (* 2 *)
    let ema_slope_gt_0 = List.map2_exn (List.tl_exn left) (List.sub left ~pos:0 ~len:(List.length left - 1))
        ~f:(fun e1 e2 -> e1.ema20 -. e2.ema20) |> List.for_all ~f:(fun e -> e > 0.) in
    (* 3 *)
    let close_to_ma20 = current.raw_data.low -. current.ma20 < 0.05 *. current.raw_data.low in
    (* 4 *)
    let last_macd_lt_current = (List.nth_exn left 2).macd *. 1.1 < current.macd in
    (* 5 *)
    let ma60_gt_k = current.ma60 > 0.9 *. current.raw_data.low in
    log (Printf.sprintf "[%s] lastmacd=%.2f, currmacd=%.2f" (Data_cursor.datestring c) (List.nth_exn left 2).macd  current.macd )>>=fun _ ->
    if ma_slope_gt_10 && ema_slope_gt_0 && close_to_ma20 && last_macd_lt_current && ma60_gt_k then
      return (Buy (c, None))
    else return (Buy_continue None)

let week_k_buy c _ = return (Buy (c, None))

let day_k_buy c _ = return (Buy (c, Some (Data_cursor.current c).raw_data.high))

let sell = V1.sell
