open Core
module C = L1.Cursor.RawData_cursor
module C2 = L1.Cursor.Data_cursor

let data_point_Nday2 (type v) (c : C2.t) n (f : Loader.Type.raw_data_elem -> v)
    : (Date.t * v) list =
  let data = C2.left_current c n in
  List.map data ~f:(fun e -> (Loader.Type.date e.raw_data, f e.raw_data))

let data_point_Nday (type v) (c : C.t) n (f : Loader.Type.raw_data_elem -> v) :
    (Date.t * v) list =
  let data = C.left_current c n in
  List.map data ~f:(fun e -> (Loader.Type.date e, f e))

(* c1 / c2 *)
let data_div_point_120d_close (c1 : C.t) (c2 : C.t) : (Date.t * float) list =
  let data1 = data_point_Nday c1 120 Loader.Type.close in
  let data2 = data_point_Nday c2 120 Loader.Type.close in
  let h = Hashtbl.of_alist_exn (module Date) data1 in
  let open Option.Monad_infix in
  List.map data2 ~f:(fun (date, v2) ->
      Hashtbl.find h date >>| fun v1 -> (date, v1 /. v2))
  |> List.filter_opt

let data_div_point_120d_close_2 (c1 : C2.t) (c2 : C2.t) : (Date.t * float) list
    =
  let data1 = data_point_Nday2 c1 120 Loader.Type.close in
  let data2 = data_point_Nday2 c2 120 Loader.Type.close in
  let h = Hashtbl.of_alist_exn (module Date) data1 in
  let open Option.Monad_infix in
  List.map data2 ~f:(fun (date, v2) ->
      Hashtbl.find h date >>| fun v1 -> (date, v1 /. v2))
  |> List.filter_opt
