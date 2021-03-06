open Core
open Poly
module C = L1_cursor.Data_cursor

let unify code (zz800 : L1_deriving.Type.Derived_data.t list)
    (deriving_data : L1_deriving.Type.Derived_data.t list) :
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
    List.map2 (L1_deriving.Op.sub_by_startdate deriving_data three_month_ago)
      (L1_deriving.Op.sub_by_startdate zz800 three_month_ago) ~f:(fun e zz800 ->
        ( assert (Date.equal e.date zz800.date);
          assert (L1_loader.Type.percent_change e.raw_data < 1.);
          { date = e.date
          ; industry
          ; rsi_golden_cross = Set.mem golden_cross_points' e.date
          ; rsi6_lt_30 = Set.mem rsi6_lt_30' e.date
          ; rsi6_lt_20 = Set.mem rsi6_lt_20' e.date
          ; ma_up
          ; ma_arranged
          ; price_less_ma20
          ; relative_strength =
              ( L1_loader.Type.percent_change e.raw_data
              -. L1_loader.Type.percent_change zz800.raw_data )
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

type datamap =
  ( string
  , L1_deriving.Type.Derived_data.t list
    * L1_deriving.Type.Derived_data.t list
    * L1_loader.Type.raw_data
  , String.comparator_witness )
  Map.t

let industry_trend (m : datamap) =
  let cm = Map.map m ~f:(fun (day_k, _, _) -> C.create_exn day_k) in
  Industry_trend.above_ma20_trend cm
  |> List.map ~f:(fun (cat, l) ->
         ( cat
         , List.map l ~f:(fun (date, s) ->
               (Date.to_string date, Score.normalize s)) ))
