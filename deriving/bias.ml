open Core
open Poly
(* https://baike.baidu.com/item/%E4%B9%96%E7%A6%BB%E7%8E%87/420286 *)
let bias_all_days n (closing_data_list: float list): float list =
  let q = Queue.create ~capacity:(n+1) () in
  List.fold ~init:[] closing_data_list ~f:(fun r a ->
      if Queue.length q >= n then
        Queue.dequeue q |> ignore;
      Queue.enqueue q a;
      let n_avg = (Queue.sum (module Float) q ~f:(fun i -> i)) /. (float_of_int (Queue.length q)) in
      ((a -. n_avg) /. n_avg) :: r) |> List.rev


let%test "test-bias" =
  let datal =
    Loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data) []
  in
  let closing_data_list = List.map datal ~f:(fun d -> d.closing) in
  let bias24_all = bias_all_days 24 closing_data_list in
  0.04523 = (Float.round_decimal ~decimal_digits:5 (List.nth_exn bias24_all 4000))

let bias n (data_list:Loader.Type.raw_data list) : (Date.t * float) list =
  match List.zip (List.map data_list ~f:(fun d -> d.date))
          (bias_all_days n (List.map data_list ~f:(fun d -> d.closing)))
  with
  | List.Or_unequal_lengths.Ok v -> v
  | List.Or_unequal_lengths.Unequal_lengths ->
    Debug.amf [%here] "length(bias of data_list) != length(data_list)";
    failwith "length(bias of data_list) != length(data_list)"
