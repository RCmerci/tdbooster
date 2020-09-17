open Std

let check_date_increase (data : Date.t array) =
  let data_arr1 = Array.subo ~len:(Array.length data - 1) data in
  let data_arr2 = Array.subo ~pos:1 data in
  Array.find (Array.zip_exn data_arr1 data_arr2) ~f:(fun (e1, e2) ->
      Date.( >= ) e1 e2)
  |> Option.map ~f:fst

module BaseData = struct
  let validate (data : L2.Data.Type.base_data_map) : string option =
    let r =
      Map.map data ~f:(fun v ->
          Array.map v ~f:(fun e -> e.date) |> check_date_increase)
    in
    match
      Map.to_alist r
      |> List.filter_map ~f:(fun e ->
             Option.map (snd e) ~f:(fun d -> (fst e, d)))
    with
    | [] -> None
    | v ->
      Some
        (List.to_string v ~f:(fun e ->
             Printf.sprintf "%s: %s" (fst e) (Date.to_string (snd e))))
end
