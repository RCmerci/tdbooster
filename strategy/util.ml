open Core

type cursor = Deriving.Type.derived_data Cursor.t

(* 状态A:
   dif, dea, macd > 0
   且 dif > macd, dea > macd
 *)
let is_status_A (c : cursor) : bool =
  let current = Cursor.current c in
  current.dif > 0. && current.dea > 0. && current.macd > 0.
  && current.macd < current.dif && current.macd < current.dea

(* 两种情况:
   1. macd 从绿柱过渡到状态A的红柱
   2. macd 红柱从左到右竹简变矮后,紧接着变高的第一根红柱
 *)
let just_enter_status_A (c : cursor) : bool =
  if not (is_status_A c) then false
  else
    let current = Cursor.current c in
    let left = Cursor.left c 2 in
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
let day_k_high_point (c : cursor) : cursor option =
  let rec aux (high_c : cursor) continue_low_macd_days (c : cursor) =
    let current = Cursor.current c in
    let high_c_data = Cursor.current high_c in
    let high_c' =
      if current.raw_data.high > high_c_data.raw_data.high then c else high_c
    in
    let continue_low_macd_days' =
      if current.macd < 0. then continue_low_macd_days + 1 else 0
    in
    if continue_low_macd_days' > 5 then Some high_c'
    else
      let c', n = Cursor.move c 1 in
      if n <> 1 then None else aux high_c' continue_low_macd_days' c'
  in
  let c', n = Cursor.move c 1 in
  if n <> 1 then None
  else aux c (if (Cursor.current c).macd < 0. then 1 else 0) c'
