open Core


let unify (deriving_data: Deriving.Type.Derived_data.t list) : Type.Attributed_data.t list =
  let two_month_ago = Date.add_months (List.last_exn deriving_data).date (-2) in
  let golden_cross_points = Rsi.golden_cross two_month_ago deriving_data in
  let golden_cross_points' = Set.of_list (module Date) golden_cross_points  in
  let rsi6_lt_40 = Rsi.low_rsi6_40 two_month_ago deriving_data in
  let rsi6_lt_30 = Rsi.low_rsi6_30 two_month_ago deriving_data in
  let rsi6_lt_40' = Set.of_list (module Date) rsi6_lt_40 in
  let rsi6_lt_30' = Set.of_list (module Date) rsi6_lt_30 in
  let attr_data = List.map (Deriving.Op.sub_by_startdate deriving_data two_month_ago)
    ~f:(fun e : Type.Attributed_data.t ->
        {
          date= e.date;
          rsi_golden_cross=Set.mem golden_cross_points' e.date;
          rsi6_lt_30=Set.mem rsi6_lt_30' e.date;
          rsi6_lt_40=Set.mem rsi6_lt_40' e.date;
        }
      )
  in
  attr_data
