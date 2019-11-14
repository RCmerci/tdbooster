open Core
open Option.Monad_infix

(* https://baike.baidu.com/item/MACD%E6%8C%87%E6%A0%87?fromtitle=MACD&fromid=3334786 *)

let dif ~ema12_list ~ema26_list : float list option =
  match
    List.map2 ema12_list ema26_list ~f:(fun ema12 ema26 -> ema12 -. ema26)
  with
  (* 第一日的 dif 为 0 *)
  | List.Or_unequal_lengths.Ok (_h :: t) ->
    Some (0. :: t)
  | List.Or_unequal_lengths.Ok _ ->
    Some [0.]
  | List.Or_unequal_lengths.Unequal_lengths ->
    Debug.amf [%here] "unequal length ema12_list ema26_list" ;
    None

let dea ~dif_list =
  match dif_list with
  | _h :: t ->
    List.fold t ~init:[0.] ~f:(fun r e ->
        ((List.nth_exn r 0 *. 0.8) +. (e *. 0.2)) :: r )
    |> List.rev
  | _ ->
    [0.]

let macd ~dif_list ~dea_list =
  match List.map2 ~f:(fun dif dea -> (dif -. dea) *. 2.) dif_list dea_list with
  | List.Or_unequal_lengths.Ok v ->
    Some v
  | List.Or_unequal_lengths.Unequal_lengths ->
    Debug.amf [%here] "unequal length dif_list dea_list" ;
    None

let%test "test-dif-dea-macd" =
  let datal =
    Loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data) []
  in
  let ema12_list = List.map (Ema.ema 12 datal) ~f:snd in
  let ema26_list = List.map (Ema.ema 26 datal) ~f:snd in
  let dif_list = Option.value_exn (dif ~ema12_list ~ema26_list) in
  let dea_list = dea ~dif_list in
  let macd_list = Option.value_exn (macd ~dif_list ~dea_list) in
  int_of_float (List.nth_exn dif_list 4000) = 20
  && int_of_float (List.nth_exn dea_list 4000) = 18
  && int_of_float (List.nth_exn macd_list 4000) = 4

let macd_dif_dea (data_list : Loader.Type.raw_data list) :
  (Date.t * float * float * float) list option =
  let time_list, ema12_list = List.unzip (Ema.ema 12 data_list) in
  let ema26_list = List.map (Ema.ema 26 data_list) ~f:snd in
  dif ~ema12_list ~ema26_list
  >>= fun dif_list ->
  let dea_list = dea ~dif_list in
  macd ~dif_list ~dea_list
  >>= fun macd_list ->
  let l1 = List.length time_list in
  let l2 = List.length macd_list in
  let l3 = List.length dif_list in
  let l4 = List.length dea_list in
  if l1 <> l2 || l1 <> l3 || l1 <> l4 then (
    Debug.amf [%here]
      "unequal length of time_list(%d), macd_list(%d), dif_list(%d), \
       dea_list(%d), raw_data_list len: %d"
      l1 l2 l3 l4 (List.length data_list) ;
    None )
  else
    let zipped =
      List.zip_exn
        (List.zip_exn (List.zip_exn time_list macd_list) dif_list)
        dea_list
    in
    Some
      (List.map zipped ~f:(fun (((t, macd), dif), dea) -> (t, macd, dif, dea)))

let%test "test-macd_dif_dea" =
  let datal =
    Loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data) []
  in
  let r = macd_dif_dea datal in
  Option.is_some r
  &&
  let date, macd, dif, dea = List.nth_exn (Option.value_exn r) 4000 in
  Date.equal date (Date.of_string "2018-06-14")
  && int_of_float macd = 4
  && int_of_float dif = 20
  && int_of_float dea = 18
