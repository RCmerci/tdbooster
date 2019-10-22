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
   从 c 开始, 连续 >1周 macd < 0 情况下, 在这之间的最高点

return (high_point_c, next_c)
   next_c: 计算下一个high_point的开始点(跳过macd<0的点)
 *)
let week_k_high_point c = k_high_point c 1

let week_k_high_point_list start end' =
  let end_t = (Data_cursor.current end').time in
  let rec aux c r =
    match week_k_high_point c with
    | None ->
        r
    | Some (high_point_c, None) ->
        if (Data_cursor.current high_point_c).time < end_t then
          high_point_c :: r
        else r
    | Some (high_point_c, Some next_c) ->
        if (Data_cursor.current high_point_c).time < end_t then
          aux next_c (high_point_c :: r)
        else r
  in
  aux start [] |> List.rev |> List.stable_dedup

let ascending_week_k_high_point_list start end' =
  match week_k_high_point_list start end' with
  | h :: t ->
      let _, r =
        List.fold t
          ~init:(h, [h])
          ~f:(fun (last_high, r) e ->
            if
              (Data_cursor.current e).raw_data.high
              > (Data_cursor.current last_high).raw_data.high
            then (e, e :: r)
            else (last_high, r) )
      in
      List.rev r
  | _ ->
      []

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

let week_k_low_point_list ?(f = week_k_high_point_list) start end' =
  match f start end' with
  | h :: t ->
      let _, r =
        List.fold t ~init:(h, []) ~f:(fun (last, r) e ->
            let low_c = week_k_low_point last e in
            (e, low_c :: r) )
      in
      List.filter_opt r |> List.rev
  | _ ->
      []

let%test_module _ =
  ( module struct
    let datal =
      Loader.From_tonghuashun_txt.read_from_string_lines
        (String.split_lines Testdata.Data.data)

    let month_k = Option.value_exn (Deriving.Unify.unify_month datal)

    let week_k = Option.value_exn (Deriving.Unify.unify_week datal)

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

    let%test "test-week_k_high_point_list" =
      let c = Data_cursor.create_exn week_k in
      let start =
        Option.value_exn
          (Data_cursor.goto_date c (Date.of_string "2017-01-07"))
      in
      let end', _ = Data_cursor.move start 99999 in
      let high_list = week_k_high_point_list start end' in
      "(2017-08-14 2018-01-15 2018-06-11 2019-04-22 2019-07-01)"
      = List.to_string
          ~f:(fun a ->
            Date.of_time (Data_cursor.current a).time
              ~zone:(Time.Zone.of_utc_offset ~hours:8)
            |> Date.to_string )
          high_list

    let%test "test-ascending_week_k_high_point_list" =
      let c = Data_cursor.create_exn week_k in
      let start =
        Option.value_exn
          (Data_cursor.goto_date c (Date.of_string "2017-01-07"))
      in
      let end', _ = Data_cursor.move start 99999 in
      let high_list = ascending_week_k_high_point_list start end' in
      "(2017-08-14:475.56 2018-01-15:773.52 2018-06-11:777.96 \
       2019-04-22:975.46 2019-07-01:1035.6)"
      = List.to_string
          ~f:(fun a ->
            ( Date.of_time (Data_cursor.current a).time
                ~zone:(Time.Zone.of_utc_offset ~hours:8)
            |> Date.to_string )
            ^ ":"
            ^ string_of_float (Data_cursor.current a).raw_data.high )
          high_list

    let%test "test-week_k_low_point_list" =
      let c = Data_cursor.create_exn week_k in
      let start =
        Option.value_exn
          (Data_cursor.goto_date c (Date.of_string "2017-01-07"))
      in
      let end', _ = Data_cursor.move start 99999 in
      let low_list = week_k_low_point_list start end' in
      "(2017-09-11:444.44 2018-04-16:619.46 2018-10-29:494.48 \
       2019-06-10:825.46)"
      = List.to_string
          ~f:(fun a ->
            ( Date.of_time (Data_cursor.current a).time
                ~zone:(Time.Zone.of_utc_offset ~hours:8)
            |> Date.to_string )
            ^ ":"
            ^ string_of_float (Data_cursor.current a).raw_data.low )
          low_list
  end )
