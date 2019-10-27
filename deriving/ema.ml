open Core

(*
https://baike.baidu.com/item/EMA/12646151
   EMA_today=α * Price_today + ( 1 - α ) * EMA_yesterday;
其中, α为平滑指数, 一般取作2/(N+1). 在计算MACD指标时, EMA计算中的N一般选取12和26天, 因此α相应为2/13和2/27.

   N: 表示计算的是N天的EMA
   CLOSING_DATA_LIST: 收盘价数据列表
 *)
let ema_all_days n (closing_data_list : float list) : float list =
  let alpha = 2. /. (float_of_int n +. 1.) in
  if List.length closing_data_list = 0 then []
  else
    let r = [List.nth_exn closing_data_list 0] in
    let nth = 1 in
    let rec aux r nth yesterday =
      match List.nth closing_data_list nth with
      | None ->
          List.rev r
      | Some today ->
          let ema_today = (alpha *. today) +. ((1. -. alpha) *. yesterday) in
          aux (ema_today :: r) (nth + 1) ema_today
    in
    aux r nth (List.nth_exn r 0)

let%test "test-ema_all" =
  let datal =
    Loader.From_tonghuashun_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data)
  in
  let closing_data_list = List.map datal ~f:(fun d -> d.closing) in
  let ema60_all = ema_all_days 60 closing_data_list in
  List.length ema60_all = List.length datal
  && int_of_float (List.nth_exn ema60_all 4000) = 705
  && int_of_float (List.nth_exn ema60_all 3000) = 86

let ema n (data_list : Loader.Type.raw_data list) : (Date.t * float) list =
  match
    List.zip
      (List.map data_list ~f:(fun d -> d.date))
      (ema_all_days n (List.map data_list ~f:(fun d -> d.closing)))
  with
  | List.Or_unequal_lengths.Ok v ->
      v
  | List.Or_unequal_lengths.Unequal_lengths ->
      Debug.amf [%here] "length(ema of data_list) != length(data_list)" ;
      failwith "length(ema of data_list) != length(data_list)"

let%test "test-ema" =
  let datal =
    Loader.From_tonghuashun_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data)
  in
  let ema60_all = ema 60 datal in
  List.length ema60_all = List.length datal
  && int_of_float (snd (List.nth_exn ema60_all 4000)) = 705
  && int_of_float (snd (List.nth_exn ema60_all 3000)) = 86
