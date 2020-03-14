open Core

let diff (deriving_data:Deriving.Type.Derived_data.t list) =
  List.map deriving_data ~f:(fun e -> (e.rsi24 -. e.rsi12, e.rsi12 -. e.rsi6))

let golden_cross diff_list =
  List.for_all diff_list ~f:(fun (d1, d2) -> d1 > 0. && d2 > 0.) &&
  List.fold_until diff_list ~init:(100,100)
    ~f:(fun (last_diff1, last_diff2) (curr_diff1, curr_diff2) ->
        if curr_diff1 < last_diff1 && curr_diff2 < last_diff2 then
          Continue_or_stop.Continue (curr_diff1, curr_diff2)
        else if curr_diff1 <= 0. && curr_diff2 <= 0. then
          Continue_or_stop.Stop ())
    ~finish:(fun r ->

      )
