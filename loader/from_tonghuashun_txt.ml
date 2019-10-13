open Core

let parse_line line : Type.raw_data option =
  let elems =
    String.split line ~on:' ' |> List.filter ~f:(fun s -> String.length s > 0)
  in
  match elems with
  | [ time_
    ; opening_
    ; high_
    ; low_
    ; closing_
    ; rose_
    ; amplitude_
    ; total_hands_
    ; amount_
    ; exchange_hands_
    ; num_of_deal_ ] -> (
    try
      let time =
        Time.parse time_ ~fmt:"%Y-%m-%d"
          ~zone:(Time.Zone.of_utc_offset ~hours:8)
      in
      let opening = Float.of_string opening_ in
      let high = Float.of_string high_ in
      let low = Float.of_string low_ in
      let closing = Float.of_string closing_ in
      let rose =
        try Float.of_string (String.chop_suffix_exn rose_ ~suffix:"%")
        with _ -> 0.
      in
      let amplitude =
        try Float.of_string (String.chop_suffix_exn amplitude_ ~suffix:"%")
        with _ -> 0.
      in
      let total_hands =
        Int64.of_string
          (String.substr_replace_all total_hands_ ~pattern:"," ~with_:"")
      in
      let amount =
        Int64.of_string
          (String.substr_replace_all amount_ ~pattern:"," ~with_:"")
      in
      let exchange_hands = Float.of_string exchange_hands_ in
      let num_of_deal =
        Int64.of_string
          (String.substr_replace_all num_of_deal_ ~pattern:"," ~with_:"")
      in
      Some
        { time
        ; opening
        ; high
        ; low
        ; closing
        ; rose
        ; amplitude
        ; total_hands
        ; amount
        ; exchange_hands
        ; num_of_deal }
    with _ -> None )
  | _ ->
      None

let read_from_string_lines lines : Type.raw_data list =
  match lines with
  | _headline :: t ->
      List.map t ~f:parse_line |> List.filter_opt
  | _ ->
      failwith "empty data"

let read_from_file file : Type.raw_data list =
  let lines = In_channel.read_lines file in
  match lines with
  | _headline :: _t ->
      read_from_string_lines lines
  | _ ->
      failwith (Printf.sprintf "empty file: %s" file)

let%test_unit "testunit-read_from_string_lines" =
  read_from_string_lines (String.split_lines Testdata.Data.data) |> ignore

let%test "test-read_from_string_lines" =
  let datal = read_from_string_lines (String.split_lines Testdata.Data.data) in
  List.length datal > 0
  &&
  let last = List.last_exn datal in
  Time.equal last.time (Time.of_string "2019-10-11 00:00:00.000000+08:00")
