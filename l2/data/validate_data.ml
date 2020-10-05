open Std

let check_not_empty (data : L1.Loader.Type.raw_data_elem list) : bool =
  List.length data <> 0

let check_date_increase (data : L1.Loader.Type.raw_data_elem list) =
  let data' = List.map data ~f:(fun e -> e.date) in
  let data_arr1 = List.sub ~pos:0 ~len:(List.length data' - 1) data' in
  let data_arr2 = List.sub ~pos:1 ~len:(List.length data' - 1) data' in
  List.find (List.zip_exn data_arr1 data_arr2) ~f:(fun (e1, e2) ->
      Date.( >= ) e1 e2)
  |> Option.is_none

module BaseData = struct
  let validate (data : (string * L1.Loader.Type.raw_data_elem list) list) :
      string list =
    let r =
      List.map data ~f:(fun (code, v) ->
          ( code
          , List.exists [ check_not_empty; check_date_increase ] ~f:(fun f ->
                not (f v)) ))
    in
    List.filter_map r ~f:(fun (code, b) ->
        if b then
          Some code
        else
          None)
end
