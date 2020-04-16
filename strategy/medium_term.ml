open Core
open Poly    
open Type
open Util
open Cursor
module O = Option.Monad_infix
open Log_warning.LogAndWarnWriter

type ctx = Data_cursor.t
type month_to_week_ctx = unit
type week_to_day_ctx = month_to_week_ctx
type day_to_sell_ctx = month_to_week_ctx

let month_k_buy c _ _:(ctx, month_to_week_ctx) buy_action Log_warning.LogAndWarnWriter.m =
  if just_enter_status_A c && (Data_cursor.current c).bias24 > 0.45 then
    log (Printf.sprintf "[%s] 刚进入 statusA (bias24>45%%)" (Data_cursor.datestring c)) >>= fun _ ->
    return (Buy (c, None, ()))
  else if will_enter_status_A c then
    log (Printf.sprintf "[%s] 将要进入 statusA" (Data_cursor.datestring c)) >>= fun _ ->
    return (Buy (c, None, ()))
  else
    return (Buy_continue None)

let week_k_buy c _ month_ctx : (ctx, week_to_day_ctx) buy_action Log_warning.LogAndWarnWriter.m =
  return (Buy (c, None, month_ctx))

let day_k_buy c _ctx _week_ctx : (ctx, day_to_sell_ctx) buy_action Log_warning.LogAndWarnWriter.m =
  return (Buy (c, Some (Loader.Type.close (Data_cursor.current c).raw_data), ()))


let sell = Long_term.sell
