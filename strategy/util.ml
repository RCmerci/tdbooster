open Core
module Data_cursor = Cursor.Make (Deriving.Type.Derived_data)

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

(* 日k高点:
   从 c 点开始, 在出现连续 >5 天 macd<0 的情况下, 在这之间的最高点
 *)
let day_k_high_point (c : Data_cursor.t) : Data_cursor.t option =
  let rec aux (high_c : Data_cursor.t) continue_low_macd_days
      (c : Data_cursor.t) =
    let current = Data_cursor.current c in
    let high_c_data = Data_cursor.current high_c in
    let high_c' =
      if current.raw_data.high > high_c_data.raw_data.high then c else high_c
    in
    let continue_low_macd_days' =
      if current.macd < 0. then continue_low_macd_days + 1 else 0
    in
    if continue_low_macd_days' > 5 then Some high_c'
    else
      let c', n = Data_cursor.move c 1 in
      if n <> 1 then None else aux high_c' continue_low_macd_days' c'
  in
  let c', n = Data_cursor.move c 1 in
  if n <> 1 then None
  else aux c (if (Data_cursor.current c).macd < 0. then 1 else 0) c'
