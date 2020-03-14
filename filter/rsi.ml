open Core

let diff (deriving_data:Deriving.Type.Derived_data.t list) =
  List.map deriving_data ~f:(fun e -> (e.rsi24 -. e.rsi12, e.rsi12 -. e.rsi6))

let golden_cross_points diff_list =
  let points = List.foldi ~init:[] diff_list
    ~f:(fun i r (diff1, diff2)  ->
        if Float.abs diff1 <= 3. && Float.abs diff2 <= 3. then
          i::r
        else r) |> List.rev
  in
  List.filter points
    ~f:(fun i ->
        let curr_diff1, curr_diff2 = List.nth_exn diff_list i in
        let before1 = List.nth diff_list (i - 1) in
        let before2 = List.nth diff_list (i - 2) in
        match (before1, before2) with
        | None, _ | _, None -> false
        | Some (before1_diff1, before1_diff2), Some (before2_diff1, before2_diff2) ->
          curr_diff1 <= before1_diff1 && curr_diff2 <= before1_diff2 &&
          curr_diff1 <= before2_diff1 && curr_diff2 <= before2_diff2)



let%test "golden_cross_points" =
  let l = golden_cross_points
      [(50., 50.);(40., 40.);(30.,30.);(20.,20.);(10.,10.);(5.,5.);(0.,0.1);(-10.,-10.);(10.,10.);(20., 20.)] in
  l = [6]




let golden_cross start_date (deriving_data: Deriving.Type.Derived_data.t list) =
  let deriving_data' = Deriving.Op.sub_by_startdate deriving_data start_date in
  let diff_list = diff deriving_data' in
  let points = golden_cross_points diff_list in
  List.map points
    ~f:(fun e ->
        let d = List.nth_exn deriving_data' e in
        d.date)
