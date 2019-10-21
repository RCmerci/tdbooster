open Core
open Option.Monad_infix
open Cursor

(* 状态A:
   dif, dea, macd > 0
   且 dif > macd, dea > macd
 *)
let is_status_A (c : Data_cursor.t) : bool =
  let current = Data_cursor.current c in
  current.dif > 0. && current.dea > 0. && current.macd > 0.
  && current.macd < current.dif && current.macd < current.dea

(* 两种情况:
   1. macd 从绿柱过渡到状态A的红柱
   2. macd 红柱从左到右竹简变矮后,紧接着变高的第一根红柱
 *)
let just_enter_status_A (c : Data_cursor.t) : bool =
  if not (is_status_A c) then false
  else
    let current = Data_cursor.current c in
    let left = Data_cursor.left c 2 in
    match left with
    | [l1; l2] ->
        if l2.macd < 0. then true
        else if l2.macd < current.macd && l1.macd > l2.macd then true
        else false
    | _ ->
        false

(* return (high_point_c, next_c)
   next_c: 计算下一个high_point的开始点(跳过macd<0的点)
 *)
let k_high_point (c : Data_cursor.t) n :
    (Data_cursor.t * Data_cursor.t option) option =
  let rec aux (high_c : Data_cursor.t) continue_low_macd_num
      (c : Data_cursor.t) =
    let current = Data_cursor.current c in
    let high_c_data = Data_cursor.current high_c in
    let high_c' =
      if current.raw_data.high > high_c_data.raw_data.high then c else high_c
    in
    let continue_low_macd_num' =
      if current.macd < 0. then continue_low_macd_num + 1 else 0
    in
    if continue_low_macd_num' > n then Some (high_c', c)
    else
      let c', n = Data_cursor.move c 1 in
      if n <> 1 then None else aux high_c' continue_low_macd_num' c'
  in
  let rec drop_macd_less_than_0 c =
    if (Data_cursor.current c).macd < 0. then
      let next_c, n = Data_cursor.move c 1 in
      if n <> 0 then drop_macd_less_than_0 next_c else None
    else Some c
  in
  let c', n = Data_cursor.move c 1 in
  if n <> 1 then None
  else
    aux c (if (Data_cursor.current c).macd < 0. then 1 else 0) c'
    >>= fun (high_c, end_c) ->
    let next_c = drop_macd_less_than_0 end_c in
    Some (high_c, next_c)

(* 日k高点:
   从 c 点开始, 在出现连续 >5 天 macd<0 的情况下, 在这之间的最高点

return (high_point_c, next_c)
   next_c: 计算下一个high_point的开始点(跳过macd<0的点)
 *)
let day_k_high_point c = k_high_point c 5

(* 周k高点:
   从 c 开始, 连续 >3周 macd < 0 情况下, 在这之间的最高点

return (high_point_c, next_c)
   next_c: 计算下一个high_point的开始点(跳过macd<0的点)
 *)
let week_k_high_point c = k_high_point c 3

(* 周k低点:
   c点到最近一个周k高点之间的最低点
*)
let week_k_low_point start end' =
  Data_cursor.fold start ~end' ~init:None ~f:(fun r e ->
      match r with
      | None ->
          Some e
      | Some low ->
          if
            (Data_cursor.current e).raw_data.low
            < (Data_cursor.current low).raw_data.low
          then Some e
          else r )

let%test_module _ =
  ( module struct
    let datal =
      Loader.From_tonghuashun_txt.read_from_string_lines
        (String.split_lines Testdata.Data.data)

    let month_k = Option.value_exn (Deriving.Unify.unify_month datal)

    let day_k = Option.value_exn (Deriving.Unify.unify_day datal)

    let%test "test-just_enter_status_A" =
      let c = Data_cursor.create_exn month_k in
      let hd =
        List.hd_exn
          (Data_cursor.fold c ~init:[] ~f:(fun r e ->
               if just_enter_status_A e then e :: r else r ))
      in
      let hd' =
        Date.of_time (Data_cursor.current hd).time
          ~zone:(Time.Zone.of_utc_offset ~hours:8)
      in
      Date.of_string "2019-04-01" = hd'

    let%test "test-day_k_high_point" =
      let c = Data_cursor.create_exn day_k in
      let c' =
        Option.value_exn
          (Data_cursor.goto_date c (Date.of_string "2019-04-01"))
      in
      let day_k_high, _ = Option.value_exn (day_k_high_point c') in
      Date.of_string "2019-04-24"
      = Date.of_time (Data_cursor.current day_k_high).time
          ~zone:(Time.Zone.of_utc_offset ~hours:8)
  end )
