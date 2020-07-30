open Core
open Poly
module C = Strategy.Cursor.Data_cursor

type cursorMap =  (String.t, C.t, String.comparator_witness) Map.t

let above_ma20_trend_aux (cm:cursorMap) (e:Loader.Type.IndustryList.one) =
  let open Option.Monad_infix in
  let l = List.map e.codes ~f:(fun code ->
      Map.find cm code >>= fun c ->
      let c' = C.move_to_last c in
      let datalist = C.left_current c' 60 in
      if  (List.length datalist <> 60)
      then None
      else
        Some (List.map datalist ~f:(fun e -> if Loader.Type.close e.raw_data > e.ma20 then (Date.to_string e.date, 1) else (Date.to_string e.date, 0)))
    ) in
  let l' = List.filter_opt l in
  assert (List.length l' > 0);
  let datelist = List.nth_exn l' 0 |> List.unzip |> fst in
  let l'' = List.map l' ~f:(fun e -> List.unzip e |> snd) in
  let totalcount = List.length (List.nth_exn l'' 0) in
  let l''' = List.fold l'' ~init:(List.init totalcount ~f:(fun _ -> 0))
      ~f:(fun r e' -> List.map2_exn r e' ~f:(+)) in
  List.zip_exn datelist (List.map l''' ~f:(fun e -> float_of_int e /. float_of_int totalcount))

let above_ma20_trend (cm:cursorMap) =
  List.map Loader.Industry.get_industry_list ~f:(fun e -> (e.category, above_ma20_trend_aux cm e))
