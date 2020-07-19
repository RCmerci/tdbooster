open Core
open Poly    
open Type
open Cursor
module O = Option.Monad_infix
open Log_warning.LogAndWarnWriter

type ctx = Data_cursor.t
type month_to_week_ctx = unit
type week_to_day_ctx = month_to_week_ctx
type day_to_sell_ctx = month_to_week_ctx

(* let month_k_buy c _ _:(ctx, month_to_week_ctx) buy_action Log_warning.LogAndWarnWriter.m =
 *   if just_enter_status_A c && (Data_cursor.current c).bias24 > 0.45 then
 *     log (Printf.sprintf "[%s] 刚进入 statusA (bias24>45%%)" (Data_cursor.datestring c)) >>= fun _ ->
 *     return (Buy (c, None, ()))
 *   else if will_enter_status_A c then
 *     log (Printf.sprintf "[%s] 将要进入 statusA" (Data_cursor.datestring c)) >>= fun _ ->
 *     return (Buy (c, None, ()))
 *   else
 *     return (Buy_continue None)
 * 
 * let week_k_buy c _ month_ctx : (ctx, week_to_day_ctx) buy_action Log_warning.LogAndWarnWriter.m =
 *   return (Buy (c, None, month_ctx))
 * 
 * let day_k_buy c _ctx _week_ctx : (ctx, day_to_sell_ctx) buy_action Log_warning.LogAndWarnWriter.m =
 *   return (Buy (c, Some (Loader.Type.close (Data_cursor.current c).raw_data), ()))
 * 
 * 
 * let sell = Long_term.sell *)


let month_k_buy c _ _:(ctx, month_to_week_ctx) buy_action Log_warning.LogAndWarnWriter.m =
  return (Buy (c, None, ()))

let week_k_buy c _ month_ctx : (ctx, week_to_day_ctx) buy_action Log_warning.LogAndWarnWriter.m =
  if Price_status.status_combine_1 c then
    log (Printf.sprintf "[%s] status_combine_1=true" (Data_cursor.datestring c)) >>= fun _ ->
    return (Buy (c, None, month_ctx))
  else
    return (Buy_continue None)

let day_k_buy c _ctx _week_ctx : (ctx, day_to_sell_ctx) buy_action Log_warning.LogAndWarnWriter.m =
  log (Printf.sprintf "[%s] buy" (Data_cursor.datestring c)) >>= fun _ ->
  return (Buy (c, Some (Loader.Type.close (Data_cursor.current c).raw_data), ()))
    
let rec sell_aux next_c week_c :(Data_cursor.t * float) option Log_warning.LogAndWarnWriter.m =
  let week_c' = Option.value_exn (Data_cursor.goto_date week_c (Data_cursor.date next_c)) in
  if Price_status.status_combine_2 week_c' then
    logf "[%s] status_combine_2" (Data_cursor.datestring week_c') >>= fun _ ->  
    return (Some (week_c', (Loader.Type.close (Data_cursor.current week_c').raw_data)))
  else
    let next_c', n = (Data_cursor.move week_c' 1) in
    if n = 0 then
      logf "[%s] no more week_c" (Data_cursor.datestring week_c') >>= fun _ ->
      return None
    else
      sell_aux next_c' week_c'

let sell ~buy_c ~buy_price _ctx _day_k week_k _month_k :(Data_cursor.t * float) option Log_warning.LogAndWarnWriter.m =
  let _ = buy_price in
  let week_c = Data_cursor.create_exn week_k in
  sell_aux buy_c week_c 

