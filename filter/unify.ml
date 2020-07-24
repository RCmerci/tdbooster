open Core
module C = Strategy.Cursor.Data_cursor

let unify (deriving_data: Deriving.Type.Derived_data.t list) : Type.Attributed_data.t list =
  let two_month_ago = Date.add_months (List.last_exn deriving_data).date (-2) in
  let golden_cross_points = Rsi.golden_cross two_month_ago deriving_data in
  let golden_cross_points' = Set.of_list (module Date) golden_cross_points  in
  let rsi6_lt_20 = Rsi.low_rsi6_20 two_month_ago deriving_data in
  let rsi6_lt_30 = Rsi.low_rsi6_30 two_month_ago deriving_data in
  let rsi6_lt_20' = Set.of_list (module Date) rsi6_lt_20 in
  let rsi6_lt_30' = Set.of_list (module Date) rsi6_lt_30 in
  let c = C.create_exn deriving_data in
  let c', _ = C.move c 999999 in (* goto last entry *)
  let ma_up = Ma_ema.ma_up c' in
  let ma_arranged = Ma_ema.ma_arrangement c' in
  let price_less_ma20 = Ma_ema.price_lessthan_ma20 c' in
  let price_before_20 = Ma_ema.price_before_20 c' in
  let price_before_60 = Ma_ema.price_before_60 c' in
  let price_before_120 = Ma_ema.price_before_120 c' in
  let attr_data = List.map (Deriving.Op.sub_by_startdate deriving_data two_month_ago)
    ~f:(fun e : Type.Attributed_data.t ->
        {
          date= e.date;
          rsi_golden_cross=Set.mem golden_cross_points' e.date;
          rsi6_lt_30=Set.mem rsi6_lt_30' e.date;
          rsi6_lt_20=Set.mem rsi6_lt_20' e.date;
          ma_up=ma_up;
          ma_arranged=ma_arranged;
          price_less_ma20=price_less_ma20;
          price_before_20=price_before_20;
          price_before_60=price_before_60;
          price_before_120=price_before_120;
        }
      )
  in
  attr_data

let marketinfo (hg: Loader.Type.RawData.t array) (gc: Loader.Type.RawData.t array) : Type.Market_data.t =
  let module C = Strategy.Cursor.RawData_cursor in
  let gc_c' = C.create_exn (Array.to_list gc) in
  let gc_c, _ = C.move gc_c' 999999 in
  let hg_c' = C.create_exn (Array.to_list hg) in
  let hg_c, _ = C.move hg_c' 999999 in
  let gc_data = Data_array.data_point_Nday gc_c 120 |> List.map ~f:(fun (d, v) -> (Date.to_string d, v)) in
  let hg_data = Data_array.data_point_Nday hg_c 120 |> List.map ~f:(fun (d, v) -> (Date.to_string d, v)) in
  let hg_div_gc = Data_array.data_div_point_120d hg_c gc_c |> List.map ~f:(fun (d, v) -> (Date.to_string d, v)) in
  {
    gc= gc_data;
    hg= hg_data;
    hg_div_gc= hg_div_gc;
  } 
