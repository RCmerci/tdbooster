open Core
open Poly
module C = L1.Cursor.Data_cursor

type cursorMap = (String.t, C.t, String.comparator_witness) Map.t

let above_ma20_trend_aux_120day (cm : cursorMap)
    (e : Loader.Type.IndustryList.one) =
  let open Option.Monad_infix in
  let l =
    List.map e.codes ~f:(fun code ->
        Map.find cm code >>= fun c ->
        let c' = C.move_to_last c in
        let datalist = C.left_current c' 120 in
        if List.length datalist <> 120 then
          None
        else
          Some
            (List.map datalist ~f:(fun e ->
                 if Loader.Type.close e.raw_data > e.ma20 then
                   (e.date, 1)
                 else
                   (e.date, 0))))
  in
  let l' = List.filter_opt l in
  assert (List.length l' > 0);
  let datelist = List.nth_exn l' 0 |> List.unzip |> fst in
  let l'' = List.map l' ~f:(fun e -> List.unzip e |> snd) in
  let datecount = List.length (List.nth_exn l'' 0) in
  let totalcount = List.length l'' in
  let l''' =
    List.fold l''
      ~init:(List.init datecount ~f:(fun _ -> 0))
      ~f:(fun r e' -> List.map2_exn r e' ~f:( + ))
  in
  List.zip_exn datelist
    (List.map l''' ~f:(fun e ->
         Score.create ~min:0. ~max:1. (float_of_int e /. float_of_int totalcount)))

let above_ma20_trend ?(auxf = above_ma20_trend_aux_120day) (cm : cursorMap) =
  let r =
    List.map Loader.Industry.get_industry_list ~f:(fun e ->
        (e.category, auxf cm e))
  in
  let sum_table = Hashtbl.create (module Date) in
  let _ =
    List.iter r ~f:(fun (_, l) ->
        List.iter l ~f:(fun (date, score) ->
            match Hashtbl.find sum_table date with
            | Some sum' -> Hashtbl.set sum_table ~key:date ~data:(score :: sum')
            | None -> Hashtbl.set sum_table ~key:date ~data:[ score ]))
  in
  let sum_alist =
    Hashtbl.map sum_table ~f:Score.sum
    |> Hashtbl.to_alist
    |> List.sort ~compare:(fun (date1, _) (date2, _) ->
           Date.compare date1 date2)
  in
  ("sum", sum_alist) :: r

let above_ma20_trend_all_aux (cm : cursorMap) (e : Loader.Type.IndustryList.one)
    =
  let sub_cm =
    List.fold e.codes
      ~init:(Map.empty (module String))
      ~f:(fun r code ->
        match Map.find cm code with
        | Some v -> Map.set r ~key:code ~data:v
        | None -> r)
  in
  let countmap =
    List.fold (Map.to_alist sub_cm)
      ~init:
        ( Map.empty (module Date)
          : (Date.t, int * int, Date.comparator_witness) Map.t )
      ~f:(fun r (_, c) ->
        let data_list = C.to_k_list c in
        List.fold data_list ~init:r ~f:(fun r e ->
            let b =
              if e.raw_data.close > e.ma20 then
                1
              else
                0
            in
            let count, sum_count =
              match Map.find r e.date with
              | Some (count, sum_count) -> (count, sum_count)
              | None -> (0, 0)
            in
            Map.set r ~key:e.date ~data:(count + b, sum_count + 1)))
  in
  Map.to_alist ~key_order:`Increasing countmap
  |> List.map ~f:(fun (date, (count, sum)) ->
         ( date
         , Score.create ~min:0. ~max:1. (float_of_int count /. float_of_int sum)
         ))

(* TODO: add test: 比较 'above_ma20_trend_all_aux' 和 'above_ma20_trend_aux_120day'
   最后 120day 数据相同 *)
