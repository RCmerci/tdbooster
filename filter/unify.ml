open Core
open Poly
module C = Strategy.Cursor.Data_cursor

let unify code (zz800 : Deriving.Type.Derived_data.t list)
    (deriving_data : Deriving.Type.Derived_data.t list) :
    Type.Attributed_data.t list =
  let three_month_ago =
    Date.add_months (List.last_exn deriving_data).date (-3)
  in
  let golden_cross_points = Rsi.golden_cross three_month_ago deriving_data in
  let golden_cross_points' = Set.of_list (module Date) golden_cross_points in
  let rsi6_lt_20 = Rsi.low_rsi6_20 three_month_ago deriving_data in
  let rsi6_lt_30 = Rsi.low_rsi6_30 three_month_ago deriving_data in
  let rsi6_lt_20' = Set.of_list (module Date) rsi6_lt_20 in
  let rsi6_lt_30' = Set.of_list (module Date) rsi6_lt_30 in
  let c = C.create_exn deriving_data in
  let c' = C.move_to_last c in
  let ma_up = Ma_ema.ma_up c' in
  let ma_arranged = Ma_ema.ma_arrangement c' in
  let price_less_ma20 = Ma_ema.price_lessthan_ma20 c' in
  let price_before_20 = Ma_ema.price_before_20 c' in
  let price_before_60 = Ma_ema.price_before_60 c' in
  let price_before_120 = Ma_ema.price_before_120 c' in
  let industry = Industry.get_industry code in
  let attr_data =
    List.map2 (Deriving.Op.sub_by_startdate deriving_data three_month_ago)
      (Deriving.Op.sub_by_startdate zz800 three_month_ago) ~f:(fun e zz800 ->
        ( assert (Date.equal e.date zz800.date);
          assert (Loader.Type.percent_change e.raw_data < 1.);
          { date = e.date
          ; industry
          ; rsi_golden_cross = Set.mem golden_cross_points' e.date
          ; rsi6_lt_30 = Set.mem rsi6_lt_30' e.date
          ; rsi6_lt_20 = Set.mem rsi6_lt_20' e.date
          ; ma_up
          ; ma_arranged
          ; price_less_ma20
          ; relative_strength =
              ( Loader.Type.percent_change e.raw_data
              -. Loader.Type.percent_change zz800.raw_data )
              *. 100.
          ; price_before_20
          ; price_before_60
          ; price_before_120
          }
          : Type.Attributed_data.t ))
  in
  match attr_data with
  | List.Or_unequal_lengths.Ok v -> v
  | List.Or_unequal_lengths.Unequal_lengths -> failwith "unequal length of data"

let marketinfo (hg : Loader.Type.RawData.t list)
    (gc : Loader.Type.RawData.t list) (cl : Loader.Type.RawData.t list) :
    Type.Market_data.t =
  let module C = Strategy.Cursor.RawData_cursor in
  let gc_c' = C.create_exn gc in
  let gc_c = C.move_to_last gc_c' in
  let hg_c' = C.create_exn hg in
  let hg_c = C.move_to_last hg_c' in
  let cl_c' = C.create_exn cl in
  let cl_c = C.move_to_last cl_c' in
  let gc_data =
    Data_array.data_point_Nday gc_c 120 Loader.Type.close
    |> List.map ~f:(fun (d, v) -> (Date.to_string d, v))
  in
  let hg_data =
    Data_array.data_point_Nday hg_c 120 Loader.Type.close
    |> List.map ~f:(fun (d, v) -> (Date.to_string d, v))
  in
  let cl_data =
    Data_array.data_point_Nday cl_c 120 Loader.Type.close
    |> List.map ~f:(fun (d, v) -> (Date.to_string d, v))
  in
  let hg_div_gc =
    Data_array.data_div_point_120d_close hg_c gc_c
    |> List.map ~f:(fun (d, v) -> (Date.to_string d, v))
  in
  let cl_div_gc =
    Data_array.data_div_point_120d_close cl_c gc_c
    |> List.map ~f:(fun (d, v) -> (Date.to_string d, v))
  in
  { gc = gc_data; hg = hg_data; cl = cl_data; hg_div_gc; cl_div_gc }

type datamap =
  ( string
  , Deriving.Type.Derived_data.t list
    * Deriving.Type.Derived_data.t list
    * Loader.Type.raw_data
  , String.comparator_witness )
  Map.t

let industry_trend (m : datamap) =
  let cm = Map.map m ~f:(fun (day_k, _, _) -> C.create_exn day_k) in
  Industry_trend.above_ma20_trend cm
