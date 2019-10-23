open Core
open Type
open Util
open Cursor
open Option.Monad_infix

type ctx = Data_cursor.t

let buy_c_low_price buy_c =
  let ratio =
    if (Data_cursor.current buy_c).raw_data.low < 0. then 1.15 else 0.85
  in
  ratio *. (Data_cursor.current buy_c).raw_data.low

let month_k_buy c _ =
  if just_enter_status_A c then (
    Debug.eprintf "just_enter_status_A: %s"
      (Time.to_string (Data_cursor.current c).time) ;
    Buy (c, None) )
  else Buy_continue None

let week_k_buy c _ = Buy (c, None)

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
      then (
        Debug.eprintf "break day_k high point: highpoint: %s, now: %s"
          (Data_cursor.datestring high_point_c)
          (Data_cursor.datestring c) ;
        Buy (c, Some (Data_cursor.current high_point_c).raw_data.high) )
      else Buy_continue (Some high_point_c)

let sell ~buy_c day_k _week_k _month_k =
  let last_c, _ = Data_cursor.move buy_c 999999 in
  let low_list =
    week_k_low_point_list ~f:ascending_week_k_high_point_list buy_c last_c
  in
  Debug.eprintf "low_list: %s"
    (List.to_string low_list ~f:(fun e ->
         let v = Data_cursor.current e in
         Data_cursor.datestring e ^ ":" ^ string_of_float v.raw_data.low )) ;
  let day_c = Data_cursor.create_exn day_k in
  match low_list with
  | [] ->
      (* 买入时候价格的85%作为第一个低点(在发现第一个周k低点之前) *)
      Data_cursor.find buy_c ~f:(fun c ->
          (Data_cursor.current c).raw_data.low < buy_c_low_price buy_c )
      >>= fun sellc -> Some (sellc, buy_c_low_price buy_c)
  | h :: _ ->
      Option.first_some
        (* 检查第一个周k低点之前的价格是否有低于买入价格的85% *)
        ( Data_cursor.find ~end':h buy_c ~f:(fun c ->
              (Data_cursor.current c).raw_data.low < buy_c_low_price buy_c )
        >>= fun e -> Some (e, buy_c_low_price buy_c) )
        (* 检查后续低于最近一个周k低点 *)
        (List.find_mapi low_list ~f:(fun ind c ->
             let low_price = (Data_cursor.current c).raw_data.low in
             let date =
               Date.of_time (Data_cursor.current c).time
                 ~zone:(Time.Zone.of_utc_offset ~hours:8)
             in
             let day_c' =
               Option.value_exn (Data_cursor.goto_date day_c date)
             in
             Data_cursor.find
               ?end':(List.nth low_list (ind + 1))
               day_c'
               ~f:(fun e ->
                 let ratio = if low_price < 0. then 1.02 else 0.98 in
                 let b =
                   (Data_cursor.current e).raw_data.low < low_price *. ratio
                 in
                 if b then
                   Debug.eprintf "break low price: %f*%f=%f, %s" ratio
                     low_price (ratio *. low_price) (Data_cursor.datestring e) ;
                 b )
             >>= fun sellc -> Some (sellc, low_price *. 0.98) ))

let%test_module _ =
  ( module struct
    let datal =
      Loader.From_tonghuashun_txt.read_from_string_lines
        (String.split_lines Testdata.Data.data)

    let month_k = Option.value_exn (Deriving.Unify.unify_month datal)

    let week_k = Option.value_exn (Deriving.Unify.unify_week datal)

    let day_k = Option.value_exn (Deriving.Unify.unify_day datal)

    let%test "test-sell" =
      let c = Data_cursor.create_exn day_k in
      let buy_c =
        Option.value_exn
          (Data_cursor.goto_date c (Date.of_string "2012-04-26"))
      in
      let sell_c, _ = Option.value_exn (sell ~buy_c day_k week_k month_k) in
      Debug.eprint (Data_cursor.to_string buy_c) ;
      Debug.eprint (Data_cursor.to_string sell_c) ;
      true
  end )
