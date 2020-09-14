open Std

module Other_info = struct
  type t =
    { gc : (Date.t * float) list
    ; hg : (Date.t * float) list
    ; cl : (Date.t * float) list
    ; hg_div_gc : (Date.t * float) list
    ; cl_div_gc : (Date.t * float) list
    }

  let get_data ~config_dir =
    let open L2.Data in
    let q = Query.BaseData.create config_dir in
    Query.BaseData.query q
      ~codes:Const.[ cl; hg; gc ]
      ~dwm:`DAY ~selector:(Query.Selector.Last 120)

  let of_basedata (basedata : L2.Data.Type.base_data_map) : t =
    assert (
      Map.mem basedata L2.Data.Const.gc
      && Map.mem basedata L2.Data.Const.hg
      && Map.mem basedata L2.Data.Const.cl );
    let gc = Map.find_exn basedata L2.Data.Const.gc in
    let hg = Map.find_exn basedata L2.Data.Const.hg in
    let cl = Map.find_exn basedata L2.Data.Const.cl in
    let gc' = Array.map ~f:(fun e -> (e.date, e.close)) gc |> Array.to_list in
    let hg' = Array.map ~f:(fun e -> (e.date, e.close)) hg |> Array.to_list in
    let cl' = Array.map ~f:(fun e -> (e.date, e.close)) cl |> Array.to_list in
    let gc_hashtbl = Hashtbl.of_alist_exn (module Date) gc' in
    let hg_hashtbl = Hashtbl.of_alist_exn (module Date) hg' in
    let cl_hashtbl = Hashtbl.of_alist_exn (module Date) cl' in
    let hg_gc =
      Hashtbl.merge gc_hashtbl hg_hashtbl ~f:(fun ~key:_ v ->
          match v with
          | `Both (v1, v2) -> Some (v2 /. v1)
          | _ -> None)
    in
    let cl_gc =
      Hashtbl.merge gc_hashtbl cl_hashtbl ~f:(fun ~key:_ v ->
          match v with
          | `Both (v1, v2) -> Some (v2 /. v1)
          | _ -> None)
    in
    let hg_div_gc =
      Hashtbl.to_alist hg_gc
      |> List.sort ~compare:(fun (a, _) (b, _) -> Date.compare a b)
    in
    let cl_div_gc =
      Hashtbl.to_alist cl_gc
      |> List.sort ~compare:(fun (a, _) (b, _) -> Date.compare a b)
    in
    { gc = gc'; hg = hg'; cl = cl'; hg_div_gc; cl_div_gc }
end
