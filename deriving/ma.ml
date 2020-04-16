open Core
open Poly
open Owl    
(* https://baike.baidu.com/item/MA指标 *)
let ma_all_days n (closing_data_list: float array) : float list =
  let q = Queue.create ~capacity:(n+1) () in
  Array.fold ~init:[] closing_data_list ~f:(fun r a ->
      if Queue.length q >= n then
        Queue.dequeue q|>ignore;
      Queue.enqueue q a;
      ((Queue.sum (module Float) q ~f:(fun i -> i)) /.
       (float_of_int (Queue.length q)))::r) |> List.rev




let%test "test-ma" = ma_all_days 3 [|1.;2.;3.;4.;5.|] = [1.; 1.5; 2.; 3.; 4.]

let%test "test-ma2" =
  let datal =
    Loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data) []
  in
  let closing_data_list = Loader.Type.close_col datal in
  let ma60_all = ma_all_days 60 closing_data_list in
  List.length ma60_all = Dataframe.row_num datal
    && int_of_float (List.nth_exn ma60_all 4000) = 692

let ma n (data_list : Loader.Type.raw_data) : (Date.t * float) list =
  match List.zip (Array.to_list (Loader.Type.date_col data_list))
          (ma_all_days n (Loader.Type.close_col data_list))
  with
  | List.Or_unequal_lengths.Ok v -> v
  | List.Or_unequal_lengths.Unequal_lengths ->
    Debug.amf [%here] "length(ma of data_list) != length(data_list)" ;
      failwith "length(ma of data_list) != length(data_list)"
