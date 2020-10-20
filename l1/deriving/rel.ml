open Std

let rel_all_days n (percentchange_data_list : float list)
    (zz800_percentchange_data_list : float list) : float list =
  let sub_length =
    List.length percentchange_data_list
    - List.length zz800_percentchange_data_list
  in
  let zz800_percentchange_data_list' =
    if sub_length < 0 then
      List.sub zz800_percentchange_data_list ~pos:(-sub_length)
        ~len:(List.length zz800_percentchange_data_list + sub_length)
    else
      List.join
        [ List.init sub_length ~f:(fun _ -> 0.); zz800_percentchange_data_list ]
  in
  let q = Queue.create ~capacity:(n + 1) () in
  List.fold2_exn ~init:[] percentchange_data_list zz800_percentchange_data_list'
    ~f:(fun r e1 e2 ->
      if Queue.length q >= n then Queue.dequeue q |> ignore;
      Queue.enqueue q (e1 -. e2);
      (100. *. Queue.sum (module Float) q ~f:ident) :: r)
  |> List.rev

let%test "test-rel" =
  rel_all_days 3 [ 1.; 2.; 3.; 4. ] [ 0.; 1.; 2.; 3.; 4. ] = [ 0.; 0.; 0.; 0. ]

let rel n (data_list : L1_loader.Type.raw_data)
    (zz800_data_list : L1_loader.Type.raw_data) : (Date.t * float) list =
  let percentchange_data_list = L1_loader.Type.percent_change_col data_list in
  let percentchange_zz800_data_list =
    L1_loader.Type.percent_change_col zz800_data_list
  in
  match
    List.zip
      (L1_loader.Type.date_col data_list)
      (rel_all_days n percentchange_data_list percentchange_zz800_data_list)
  with
  | List.Or_unequal_lengths.Ok v -> v
  | List.Or_unequal_lengths.Unequal_lengths ->
    Debug.amf [%here] "length(rel of data_list) != length(data_list)";
    failwith "length(rel of data_list) != length(data_list)"
