open Core
open Type
open Util
open Cursor
module O = Option.Monad_infix
open Log_warning.LogAndWarnWriter

type ctx = Data_cursor.t

let buy_c_low_price buy_price =
  let ratio = if buy_price < 0. then 1.15 else 0.85 in
  ratio *. buy_price

let month_k_buy c _ : ctx buy_action Log_warning.LogAndWarnWriter.m =
  let open Log_warning.LogAndWarnWriter in
  if just_enter_status_A c then
    log (Printf.sprintf "[%s] 刚进入 statusA" (Data_cursor.datestring c)) >>= fun _ ->
    return (Buy (c, None))
  else return (Buy_continue None)

let week_k_buy c _ : ctx buy_action Log_warning.LogAndWarnWriter.m =
  return (Buy (c, None))

let day_k_buy c ctx : ctx buy_action Log_warning.LogAndWarnWriter.m =
  match ctx with
  | None -> (
      let r =
        Option.bind (day_k_high_point c)
          ~f:(fun (high_point_c, next_c) ->
              match next_c with
              | None ->
                Some (Buy_continue (Some high_point_c))
              | Some next_c' ->
                Some (Buy_skip_to (Some high_point_c, next_c')))
      in
      match r with None -> return (Buy_continue None) | Some v -> return v )
  | Some high_point_c ->
    (* 如果90天还没有突破日k高点,则放弃这次入场 *)
    if Date.diff (Data_cursor.date c) (Data_cursor.date high_point_c) > 90
    then log(Printf.sprintf "[%s] 90天没有突破日k高点(%s), 放弃"
               (Data_cursor.datestring c) (Data_cursor.datestring high_point_c)) >>= fun _ ->
      return ( Buy_quit c)
    else if
      (Data_cursor.current high_point_c).raw_data.high
      < (Data_cursor.current c).raw_data.high
    then (
      log (Printf.sprintf "[%s] 突破日k高点(%s),=买入="
             (Data_cursor.datestring c) (Data_cursor.datestring high_point_c)) >>= fun _ ->
      return (Buy (c, Some (Data_cursor.current high_point_c).raw_data.high) ))
    else return (Buy_continue (Some high_point_c))

let sell ~buy_c ~buy_price day_k _week_k _month_k :(Data_cursor.t * float) option Log_warning.LogAndWarnWriter.m =
  let last_c, _ = Data_cursor.move buy_c 999999 in
  let low_list =
    week_k_low_point_list ~f:ascending_week_k_high_point_list buy_c last_c
  in
  let day_c = Data_cursor.create_exn day_k in
  match low_list with
  | [] ->
    (* 买入时候价格的85%作为第一个低点(在发现第一个周k低点之前) *)
    Option.value_map
      (Option.bind (Data_cursor.find buy_c ~f:(fun c ->
           (Data_cursor.current c).raw_data.low < buy_c_low_price buy_price ))
          ~f:(fun sellc -> Some (sellc, buy_c_low_price buy_price,
                                 Printf.sprintf "[%s] 低于买入价格(%f)*85%%, 卖出" (Data_cursor.datestring sellc) buy_price)))
      ~default:(return None)
      ~f:(fun (sellc, sellp, l) -> (log l >>= fun _ -> return (Some(sellc, sellp))))
  | h :: _ ->
    Option.first_some
      (* 检查第一个周k低点之前的价格是否有低于买入价格的85% *)
      (Option.bind
         (Data_cursor.find ~end':h buy_c ~f:(fun c ->
              (Data_cursor.current c).raw_data.low < buy_c_low_price buy_price))
         ~f:(fun e -> Some (e, buy_c_low_price buy_price, Printf.sprintf "[%s] 低于买入价格(%f)*85%%, 卖出"
                              (Data_cursor.datestring e) buy_price)) )

      (* 检查后续低于最近一个周k低点 *)
      (List.find_mapi low_list ~f:(fun ind c ->
           let low_price = (Data_cursor.current c).raw_data.low in
           let ratio = if low_price < 0. then 1.02 else 0.98 in
           let date =
             (Data_cursor.current c).date
           in
           let day_c' =
             Option.value_exn (Data_cursor.goto_date day_c date)
           in
           Option.bind (Data_cursor.find
                          ?end':(List.nth low_list (ind + 1))
                          day_c'
                          ~f:(fun e ->
                              (Data_cursor.current e).raw_data.low < low_price *. ratio))
             ~f:(fun sellc -> Some (sellc, low_price *. 0.98, Printf.sprintf "[%s] 低于周k低点价格(%s/%f)*, 卖出"
                                      (Data_cursor.datestring sellc) (Data_cursor.datestring c) (low_price) ))))
    |> Option.value_map ~default:(return None) ~f:(fun (sellc, sellp, l) -> log l>>=fun _ -> return (Some(sellc, sellp)))
