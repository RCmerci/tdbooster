open Core
open Type
open Util
open Cursor
open Option.Monad_infix

let month_k_buy c _ =
  if just_enter_status_A c then Buy c else Buy_continue None

let week_k_buy c _ = Buy c

let day_k_buy c ctx =
  match ctx with
  | None -> (
      let r =
        day_k_high_point c
        >>= fun (high_point_c, next_c) ->
        match next_c with
        | None ->
            Some (Buy_continue (Some high_point_c))
        | Some next_c' ->
            Some (Buy_skip_to (Some high_point_c, next_c'))
      in
      match r with None -> Buy_continue None | Some v -> v )
  | Some high_point_c ->
      if
        (Data_cursor.current high_point_c).raw_data.high
        < (Data_cursor.current c).raw_data.high
      then Buy c
      else Buy_continue (Some high_point_c)

(* let sell ~buy_c =
 *   week_k_high_point buy_c
 *   >>= fun (high_point_c, next_c) ->
 *   if high_point_c.current = buy_c.current then
 *     (match week_k_high_point next_c with
 *        None -> Some Sell_keep
 *      | Some (high_point_c', next_c') ->
 *
 *
 *     ) *)
