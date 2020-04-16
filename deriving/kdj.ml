open Core
open Poly
open Owl
    
let rsv c l h = (c -. l) /. (h -. l) *. 100.0

let kdj_all_days n (data: Loader.Type.raw_data) =
  let high_q = Queue.create ~capacity:(n+1) () in
  let low_q = Queue.create ~capacity:(n+1) () in
  let hl_list = Loader.Type.fold data ~init:[]
      ~f:(fun r e ->
          Queue.enqueue high_q (Loader.Type.high e);
          if Queue.length high_q > n then
            Queue.dequeue high_q|> ignore;
          Queue.enqueue low_q (Loader.Type.low e);
          if Queue.length low_q > n then
            Queue.dequeue low_q |> ignore;
          let max' = Queue.max_elt high_q ~compare:(fun a b -> if a -. b > 0. then 1 else (-1)) in
          let min' = Queue.max_elt low_q ~compare:(fun a b -> if b -. a > 0. then 1 else (-1)) in
          let max = Option.value_exn max' in
          let min = Option.value_exn min' in
          (max, min)::r) |> List.rev |> Array.of_list in
  let rsv_list =  Loader.Type.map_with_array data hl_list ~f:(fun e (high, low) -> rsv (Loader.Type.close e) low high) in
  let k_list = Array.fold rsv_list ~init:(50., [])
      ~f:(fun (last_k, r) rsv ->
          let k = (2. /. 3.) *. last_k +. (1. /. 3.) *. rsv in
          (k, k::r)) |> snd |> List.rev
  in
  let d_list = List.fold k_list ~init:(50., [])
      ~f:(fun (last_d, r) k ->
          let d = (2. /. 3.) *. last_d +. (1. /. 3.) *. k in
          (d, d::r)) |> snd |> List.rev
  in
  let j_list = match List.map2 k_list d_list ~f:(fun k d -> 3. *. k -. 2. *. d) with
    | List.Or_unequal_lengths.Ok e -> e
    | List.Or_unequal_lengths.Unequal_lengths -> failwith "kdj_all_days"
  in
  let kdj_list = match List.map3 k_list d_list j_list ~f:(fun k d j -> (k,d,j)) with
  | List.Or_unequal_lengths.Ok e -> e
  | List.Or_unequal_lengths.Unequal_lengths -> failwith "kdj_all_days"
  in
  match List.zip (Array.to_list (Loader.Type.date_col data)) kdj_list with
  | List.Or_unequal_lengths.Ok e -> e
  | List.Or_unequal_lengths.Unequal_lengths -> failwith "kdj_all_days"


let%test "test-kdj" =
  let datal =
    Loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data) []
  in
  let datal' = Dataframe.tail 4000  datal (* ~pos:4000 ~len:(List.length datal - 4001)  *)in
  List.last_exn (kdj_all_days 9 datal') |> fun (_, (k,d,j))->
  int_of_float k = 65 && int_of_float d = 65 && int_of_float j = 64

let kdj n (data_list:Loader.Type.raw_data): (Date.t * (float * float * float)) list = kdj_all_days n data_list
