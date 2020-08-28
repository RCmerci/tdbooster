open Core
open Poly

let diff open' close high low =
  let up, down =
    if open' > close then
      (open', close)
    else
      (close, open')
  in
  let d1 = high -. up in
  let d2 = up -. down in
  let d3 = down -. low in
  (d1, d2, d3)
  (*
   U: up
   D: down
  *)

  (*
   |  |
   |  |
   U  D
   |  |
  *)[@ocamlformat "disable"]

let status1 ?(ratio = 2.) open' close high low =
  assert (ratio > 1.);
  let d1, d2, d3 = diff open' close high low in
  d1 /. d2 > ratio
  && d1 /. d3 > ratio

              (*
              |  |
              |  |
              U  D
              |  |
              |  |
              *)[@ocamlformat "disable"]

let status2 ?(ratio = 2.) open' close high low =
  assert (ratio > 1.);
  let d1, d2, d3 = diff open' close high low in
  d1 /. d2 > ratio
  && d3 /. d2 > ratio

              (*
              |  |
              U  D
              |  |
              |  |
              *)[@ocamlformat "disable"]

let status3 ?(ratio = 2.) open' close high low =
  assert (ratio > 1.);
  let d1, d2, d3 = diff open' close high low in
  d3 /. d1 > ratio && d3 /. d2 > ratio

                               (*
                               |
                               U
                               U
                               |
                               *)[@ocamlformat "disable"]

let status4 ?(ratio = 2.) open' close high low =
  assert (ratio > 1.);
  let d1, d2, d3 = diff open' close high low in
  open' < close
  && d2 /. d1 > ratio
  && d2 /. d3 > ratio


              (*
              |
              D
              D
              |
              *)[@ocamlformat "disable"]

let status5 ?(ratio = 2.) open' close high low =
  assert (ratio > 1.);
  let d1, d2, d3 = diff open' close high low in
  open' > close
  && d2 /. d1 > ratio
  && d2 /. d3
     > ratio




     (*
     .     \    c
     .      \  /
     .       \/  |         |
     .        |  \-------> U
     .        V            U
     .                     |
     .    |  |  |  |
     .    |or|orU  D
     .    U  D  |or|
     .    |  |  |  |
     *)[@ocamlformat "disable"]

let status_combine_1 c =
  let open Loader.Type in
  let l = Cursor.Data_cursor.left c 4 @ [ Cursor.Data_cursor.current c ] in
  let fst4 = List.sub l ~pos:0 ~len:4 in
  let is_down_head =
    List.fold_until fst4 ~init:99999.
      ~f:(fun r e ->
        if Loader.Type.close e.raw_data < r then
          Continue_or_stop.Continue (Loader.Type.close e.raw_data)
        else
          Continue_or_stop.Stop false)
      ~finish:(fun _ -> true)
  in
  let e4 = List.nth_exn l 3 in
  let e4' =
    List.map [ status1; status2; status3 ] ~f:(fun f ->
        f (opening e4.raw_data) (close e4.raw_data) (high e4.raw_data)
          (low e4.raw_data))
    |> List.find ~f:ident |> Option.is_some
  in
  let e5 = List.nth_exn l 4 in
  let e5' =
    status4 (opening e5.raw_data) (close e5.raw_data) (high e5.raw_data)
      (low e5.raw_data)
  in
  let e45 = close e4.raw_data < close e5.raw_data in
  is_down_head && e4' && e5'
  && e45

  (*
  c
  |  |
  |  |
  k1 k2

  - k2.close < k1.close - 5%
  - k2.open - k2.close > 4%
  - mid(k1.open, k1.close) > k2.close
  *)[@ocamlformat "disable"]

let status_combine_2 c =
  let k1, n = Cursor.Data_cursor.move c (-1) in
  let k2 = c in
  if n <> -1 then
    false
  else
    let k1' = Cursor.Data_cursor.current k1 in
    let k2' = Cursor.Data_cursor.current k2 in
    let open Loader.Type in
    let diff1 =
      (close k1'.raw_data -. close k2'.raw_data) /. close k2'.raw_data
    in
    let diff2 =
      (opening k2'.raw_data -. close k2'.raw_data) /. close k2'.raw_data
    in
    let diff3 =
      ((opening k1'.raw_data +. close k1'.raw_data) /. 2.) -. close k2'.raw_data
    in
    diff1 > 0.05 && diff2 > 0.04 && diff3 > 0.

let%test "test-status1" = status1 10. 11. 13.1 9.

let%test "test-status2" = status2 10. 11. 13.1 7.9

let%test "test-status3" = status3 10. 11. 12. 7.9

let%test "test-status4" = status4 10. 12.1 13. 9.

let%test "test-status5" = status5 12.1 10. 13. 9.

let%test_module _ =
  ( module struct
    let a1 : Deriving.Type.Derived_data.t =
      { date = Date.of_string "2012-01-01"
      ; (* [|"date";"open";"high";"low";"close";"ttm";"days"|] *)
        raw_data =
          { date = Date.of_string "2012-01-01"
          ; opening = 1.
          ; high = 1.
          ; low = 1.
          ; close = 10.
          ; ttm = 0.
          ; days = 1
          ; percent_change = 0.
          }
      ; ma20 = 0.
      ; ma60 = 0.
      ; ma120 = 0.
      ; ema12 = 0.
      ; ema20 = 0.
      ; ema26 = 0.
      ; ema60 = 0.
      ; ema120 = 0.
      ; dif = 0.
      ; dea = 0.
      ; macd = 0.
      ; bias24 = 0.
      ; rsi6 = 0.
      ; rsi12 = 0.
      ; rsi24 = 0.
      ; kdj933 = (0., 0., 0.)
      }

    let a2 =
      { a1 with
        date = Date.of_string "2012-01-02"
      ; raw_data =
          { date = Date.of_string "2012-01-02"
          ; opening = 1.
          ; high = 1.
          ; low = 1.
          ; close = 9.
          ; ttm = 0.
          ; days = 1
          ; percent_change = 0.
          }
      }

    let a3 =
      { a1 with
        date = Date.of_string "2012-01-03"
      ; raw_data =
          { date = Date.of_string "2012-01-03"
          ; opening = 1.
          ; high = 1.
          ; low = 1.
          ; close = 8.
          ; ttm = 0.
          ; days = 1
          ; percent_change = 0.
          }
      }

    let a4 =
      { a1 with
        date = Date.of_string "2012-01-04"
      ; raw_data =
          { date = Date.of_string "2012-01-04"
          ; opening = 6.
          ; high = 10.
          ; low = 3.
          ; close = 7.
          ; ttm = 0.
          ; days = 1
          ; percent_change = 0.
          }
      }

    let a5 =
      { a1 with
        date = Date.of_string "2012-01-05"
      ; raw_data =
          { date = Date.of_string "2012-01-05"
          ; opening = 5.
          ; high = 11.
          ; low = 4.
          ; close = 10.
          ; ttm = 0.
          ; days = 1
          ; percent_change = 0.
          }
      }

    let b1 =
      { a1 with
        date = Date.of_string "2012-01-05"
      ; raw_data =
          { date = Date.of_string "2012-01-05"
          ; opening = 9.
          ; high = 11.
          ; low = 4.
          ; close = 10.
          ; ttm = 0.
          ; days = 1
          ; percent_change = 0.
          }
      }

    let b2 =
      { a1 with
        date = Date.of_string "2012-01-06"
      ; raw_data =
          { date = Date.of_string "2012-01-06"
          ; opening = 11.
          ; high = 11.
          ; low = 4.
          ; close = 9.
          ; ttm = 0.
          ; days = 1
          ; percent_change = 0.
          }
      }

    let c1 = Option.value_exn (Cursor.Data_cursor.create [ a1; a2; a3; a4; a5 ])

    let c2 = Option.value_exn (Cursor.Data_cursor.create [ b1; b2 ])

    let%test "test-status_combine_1" = status_combine_1 c1

    let%test "test-status_combine_2" =
      let c2', _ = Cursor.Data_cursor.move c2 1 in
      status_combine_2 c2'
  end )
