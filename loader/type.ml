open Core
open Owl     

(* date: 日k:当天, 周k:周第一天, 月k:月第一天 
 * days: 日k: 1, 周k: 本周交易天数, 月k: 本月交易天数 
 * [|"date";"open";"high";"low";"close";"ttm";"days"|]  *)
type raw_data = Dataframe.t

let date_col t = Dataframe.(unpack_string_series (get_col t 0)) |> Array.map ~f:Date.of_string

let opening_col t = Dataframe.(unpack_float_series (get_col t 1))

let high_col t = Dataframe.(unpack_float_series (get_col t 2))

let low_col t = Dataframe.(unpack_float_series (get_col t 3))

let close_col t = Dataframe.(unpack_float_series (get_col t 4))

let ttm_col t = Dataframe.(unpack_float_series (get_col t 5))

let days_col t = Dataframe.(unpack_int_series (get_col t 6))


let date_string t = match Array.nget t 0 with
  | Dataframe.String d -> d
  | _ -> failwith "unreachable"
           
let date t = match Array.nget t 0 with
  | Dataframe.String d -> Date.of_string d
  | _ -> failwith "unreachable"
           
let opening t = match Array.nget t 1 with
  | Dataframe.Float f -> f
  | _ -> failwith "unreachable"

let high t = match Array.nget t 2 with
  | Dataframe.Float f -> f
  | _ -> failwith "unreachable"

let low t = match Array.nget t 3 with
  | Dataframe.Float f -> f
  | _ -> failwith "unreachable"

let close t = match Array.nget t 4 with
  | Dataframe.Float f -> f
  | _ -> failwith "unreachable"

let ttm t = match Array.nget t 5 with
  | Dataframe.Float f -> f
  | _ -> failwith "unreachable"

let days t = match Array.nget t 6 with
  | Dataframe.Int i -> i
  | _ -> failwith "unreachable"


let set_date t v =  Array.nset t 0 (Dataframe.String (Date.to_string v)) 
let set_opening t v =  Array.nset t 1 (Dataframe.Float v) 
let set_high t v =  Array.nset t 2 (Dataframe.Float v) 
let set_low t v =  Array.nset t 3 (Dataframe.Float v) 
let set_close t v =  Array.nset t 4 (Dataframe.Float v) 
let set_ttm t v =  Array.nset t 5 (Dataframe.Float v) 
let set_days t v =  Array.nset t 6 (Dataframe.Int v) 


let make_raw_data (data:Dataframe.elt array list) =
  let t = Dataframe.make [|"date";"open";"high";"low";"close";"ttm";"days"|] in
  List.iter data ~f:(fun d ->Dataframe.append_row t d);
  t
  
let fold t ~init ~f =
  let r = ref init in
  Dataframe.iter_row (fun e ->
      let r' = f !r e in
      r := r';
    ) t;
  !r

let map_with_array t l ~f =
  assert (Array.length l = Dataframe.row_num t);
  Array.map2_exn (Dataframe.to_rows t) l ~f
