open Std

module Basedata_info = struct
  type attr = Public_type.Marketinfo_basedata_info.attr

  type elem = Public_type.Marketinfo_basedata_info.elem

  type t = Public_type.Marketinfo_basedata_info.t

  let get_data ~config_dir ~custom_codes =
    let open L2.Data in
    let q = Query.BaseData.create config_dir in
    let q' = Query.DerivedData.create config_dir in
    let basedata =
      Query.BaseData.query q
        ~codes:(L2.Data.Const.zz800 :: custom_codes)
        ~dwm:`DAY ~selector:(Query.Selector.Last 200)
    in
    let derived_data =
      Query.DerivedData.query q'
        ~codes:(L2.Data.Const.zz800 :: custom_codes)
        ~dwm:`DAY ~selector:(Query.Selector.Last 200)
    in
    (basedata, derived_data)

  let of_data (basedata : L2.Data.Type.base_data_map)
      (derived_data : L2.Data.Type.derived_data_map) : t =
    let f1 (derived_data : L2.Data.Type.derived_data_elem array) =
      Array.map derived_data ~f:(fun e ->
          let ma_arranged = e.ma20 > e.ma60 && e.ma60 > e.ma120 in
          (e.date, ma_arranged))
      |> Array.to_list
    in
    assert (Map.mem basedata L2.Data.Const.zz800);
    let zz800 = Map.find_exn basedata L2.Data.Const.zz800 in
    let f2 (basedata : L2.Data.Type.base_data_elem array) =
      assert (Array.length basedata = Array.length zz800);
      Array.map2_exn basedata zz800 ~f:(fun e1 e2 ->
          assert (e1.date = e2.date);
          (e1.date, (e1.percent_change -. e2.percent_change) *. 100.))
      |> Array.to_list
    in
    let r1 = Map.map derived_data ~f:f1 in
    let r2 = Map.map basedata ~f:f2 in
    Map.merge r1 r2 ~f:(fun ~key:code v ->
        match v with
        | `Both (v1, v2) ->
          let h1 = Hashtbl.of_alist_exn (module Date) v1 in
          let h2 = Hashtbl.of_alist_exn (module Date) v2 in
          let attrlist =
            Hashtbl.merge h1 h2 ~f:(fun ~key:d v ->
                match v with
                | `Both (ma_arranged, relative_strength) ->
                  Some (ma_arranged, relative_strength)
                | `Left _ ->
                  Debug.eprintf "left:%s" (Date.to_string d);
                  None
                | `Right _ ->
                  Debug.eprintf "right:%s, code:%s" (Date.to_string d) code;
                  None)
            |> Hashtbl.to_alist
            |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
            |> List.map
                 ~f:(fun (date, (ma_arranged, relative_strength)) : attr ->
                   { date; ma_arranged; relative_strength })
          in
          let industry = L2.Data.Const.get_industry_opt code in
          Some ({ code; industry; day_data = attrlist } : elem)
        | _ -> None)
    |> Map.to_alist |> List.map ~f:snd
end

module Other_info = struct
  type t =
    { gc : (Date.t * float) list
    ; hg : (Date.t * float) list
    ; cl : (Date.t * float) list
    ; hg_div_gc : (Date.t * float) list
    ; cl_div_gc : (Date.t * float) list
    }
  [@@deriving yojson]

  let get_data ~config_dir =
    let open L2.Data in
    let q = Query.BaseData.create config_dir in
    Query.BaseData.query q
      ~codes:Const.[ cl; hg; gc ]
      ~dwm:`DAY ~selector:(Query.Selector.Last 120)

  let of_data (basedata : L2.Data.Type.base_data_map) : t =
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

module Industry_trend_info = struct
  (* t = (industry * (date * percent) list) list *)
  type t = (string * (Date.t * float) list) list [@@deriving yojson]

  let get_data ~config_dir =
    let open L2.Data in
    let q = Query.IndustryTrendData.create config_dir in
    Query.IndustryTrendData.query q
      ~industries:("sum" :: Const.industry_cat_list)
      ~dwm:`DAY ~selector:(Query.Selector.Last 150)

  let of_data (industry_trend_info : L2.Data.Type.industry_trend_data_map) : t =
    Map.map industry_trend_info ~f:(fun e ->
        Array.map e ~f:(fun e -> (e.date, e.above_ma20_percent))
        |> Array.to_list)
    |> Map.to_alist
end
