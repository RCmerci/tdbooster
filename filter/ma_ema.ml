open Core
open Poly
module C = L1.Cursor.Data_cursor

(* ma20, ma60, ma120 最近3点为上升趋势 *)
let ma_up (c : C.t) : bool =
  let data = C.left_current c 3 in
  if List.length data <> 3 then
    false
  else
    let d1 = List.nth_exn data 0 in
    let d2 = List.nth_exn data 1 in
    let d3 = List.nth_exn data 2 in
    d1.ma20 < d2.ma20 && d2.ma20 < d3.ma20 && d1.ma60 < d2.ma60
    && d2.ma60 < d3.ma60 && d1.ma120 < d2.ma120 && d2.ma120 < d3.ma120

(* ma20 > ma60 > ma120 *)
let ma_arrangement (c : C.t) : bool =
  let data = C.current c in
  data.ma20 > data.ma60 && data.ma60 > data.ma120

(* price < ma20 *)
let price_lessthan_ma20 (c : C.t) : bool =
  let data = C.current c in
  Loader.Type.close data.raw_data < data.ma20

let price_before_N (c : C.t) n : float * string =
  let c', _ = C.move c (-n) in
  let data = C.current c' in
  let datestring = C.datestring c' in
  (Loader.Type.close data.raw_data, datestring)

(* 20 天/周 之前的价格/时间 *)
let price_before_20 c = price_before_N c 20

let price_before_60 c = price_before_N c 60

let price_before_120 c = price_before_N c 120
