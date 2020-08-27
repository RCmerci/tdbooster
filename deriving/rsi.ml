open Core
open Poly


let smma n (closing_data_list : float list) : float list =
  let alpha = 1. /. (float_of_int n) in
  if List.length closing_data_list = 0 then []
  else
    let r = [List.nth_exn closing_data_list 0] in
    let nth = 1 in
    let rec aux r nth yesterday =
      if List.length closing_data_list <= nth then
        List.rev r
      else 
        let today = List.nth_exn closing_data_list nth in
        let ema_today = (alpha *. today) +. ((1. -. alpha) *. yesterday) in
        aux (ema_today :: r) (nth + 1) ema_today
    in
    aux r nth (List.nth_exn r 0)

let rsi_all_days n (closing_data_list:float list): float list =
  let l = List.map2_exn
      (List.sub closing_data_list ~pos:0 ~len:(List.length closing_data_list - 1))
      (List.sub closing_data_list ~pos:1 ~len:(List.length closing_data_list - 1))
      ~f:(fun a b -> if a > b then (0., a  -. b) else (b -. a, 0.)) in
  let up, down = List.unzip l in
  let up' = smma n up in
  let down' = smma n down in
  let rs = List.map2_exn up' down' ~f:(fun u d -> u /. d) in
  let rsi = List.map rs ~f:(fun rs' -> 100. -. (100. /. (1. +. rs'))) in
  0. :: rsi


let%test "test-rsi" =
  let datal =
    Loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data) [] in
  let closing_data_list = Loader.Type.close_col datal in
  let rsi6_all = rsi_all_days 6 closing_data_list in
  60.763000 = (Float.round_decimal (List.last_exn rsi6_all) ~decimal_digits:3)


let rsi n (data_list:Loader.Type.raw_data): (Date.t * float) list =
  match List.zip
          (Loader.Type.date_col data_list)
          (rsi_all_days n (Loader.Type.close_col data_list))
  with
  | List.Or_unequal_lengths.Ok v -> v
  | List.Or_unequal_lengths.Unequal_lengths ->
    Debug.amf [%here] "rsi";
    failwith "rsi"
