open Core

let parse_line line : Type.raw_data_elem option =
  let elems =
    String.split line ~on:' ' |> List.filter ~f:(fun s -> String.length s > 0)
  in
  match elems with
  | time_ :: opening_ :: high_ :: low_ :: closing_ :: _t -> (
    try
      let time =
        Time.parse time_ ~fmt:"%Y-%m-%d"
          ~zone:(Time.Zone.of_utc_offset ~hours:8)
      in
      let date = Date.of_time time ~zone:(Time.Zone.of_utc_offset ~hours:8) in
      let opening = Float.of_string opening_ in
      let high = Float.of_string high_ in
      let low = Float.of_string low_ in
      let close = Float.of_string closing_ in
      let days = 1 in
      Some {
        date;
        opening;
        high;
        low;
        close;
        ttm=0.;
        days;
        percent_change=0.;
      }
    with _ -> None )
  | _ ->
    None

let parse_ttm_line line : (Date.t * float) option =
  let elems = String.split line ~on:' ' |> List.filter ~f:(fun s -> String.length s > 0)
  in
  match elems with
  | time_ :: _ :: pettm_ :: _t -> begin
      try
        let date = Date.parse ~fmt:"%Y-%m-%d" time_ in
        let ttm = Float.of_string pettm_ in
        Some (date, ttm)
      with _ -> None
    end
  | _ -> None

let read_from_string_lines lines ttm_lines : Type.raw_data =
  match (lines, ttm_lines) with
  | (_h :: t, _ :: t') | (_h :: t, t')->
    let h = match (Hashtbl.create_mapped (module Date)
                     ~get_key:(fun (x:Type.raw_data_elem) -> x.date) ~get_data:(fun x -> x)
                     (List.map t ~f:parse_line |> List.filter_opt)) with
    | `Duplicate_keys _ -> failwith "Hashtbl.create_mapped"
    | `Ok h -> h
    in
    let h' = match (Hashtbl.create_mapped (module Date)
                      ~get_key:(fun x -> fst x) ~get_data:(fun x -> snd x)
                      (List.map t' ~f:parse_ttm_line |> List.filter_opt)) with
    | `Duplicate_keys _ -> failwith "Hashtbl.create_mapped ttm"
    | `Ok h -> h
    in
    Hashtbl.merge h h' ~f:(fun ~key:_ -> function
        | `Left x -> Some x
        | `Both (x, y) -> Some {x with ttm=y}
        | _ -> None)
    |> Hashtbl.to_alist |> List.unzip |> snd |> List.sort
      ~compare:(fun (a:Type.raw_data_elem) (b:Type.raw_data_elem) -> Date.compare a.date b.date)
  | _ ->
      failwith "empty data"



let read_from_file file ttm_file : Type.raw_data  =
  let lines = In_channel.read_lines file in
  let ttm_lines = In_channel.read_lines ttm_file in
  match (lines, ttm_lines) with
  | (_headline :: _t, _headline' :: _t') ->
    read_from_string_lines lines ttm_lines
  | _ ->
      failwith (Printf.sprintf "empty file or ttm_file: %s, %s" file ttm_file)

let%test_unit "testunit-read_from_string_lines" =
  read_from_string_lines (String.split_lines Testdata.Data.data) [] |> ignore

let%test "test-read_from_string_lines" =
  let datal = read_from_string_lines (String.split_lines Testdata.Data.data) (String.split_lines Testdata.Data.ttm_data) in
  List.length datal > 0
  &&
  let last = List.last_exn datal in
  Date.equal (Type.date last) (Date.of_string "2019-10-11")
