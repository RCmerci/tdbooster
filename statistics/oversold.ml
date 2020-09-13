open Core
open Poly

let f code (data : L1.Deriving.Type.Derived_data.t list) : Type.display_struct =
  let open Option.Monad_infix in
  let six_month_ago =
    Date.add_months (Date.today ~zone:(Time.Zone.of_utc_offset ~hours:8)) (-6)
  in
  let sub_data = L1.Deriving.Op.sub_by_startdate data six_month_ago in
  let low_rsi6_list =
    List.filter_mapi sub_data ~f:(fun i e ->
        if e.rsi6 < 20. then
          Some i
        else
          None)
  in
  let down_lt_5 =
    List.filter_map low_rsi6_list ~f:(fun i ->
        let n = List.nth_exn sub_data i in
        List.nth sub_data (i + 1) >>= fun n_1 ->
        let ratio =
          (L1.Loader.Type.close n_1.raw_data -. L1.Loader.Type.close n.raw_data)
          /. L1.Loader.Type.close n.raw_data
        in
        if ratio < -0.05 then
          Some (i + 1)
        else
          None)
  in
  let updown =
    List.filter_map down_lt_5 ~f:(fun i ->
        let n = List.nth_exn sub_data i in
        List.nth sub_data (i + 1) >>= fun n_1 ->
        if L1.Loader.Type.close n.raw_data < L1.Loader.Type.close n_1.raw_data
        then
          Some (`UP, n_1.date)
        else
          Some (`DOWN, n_1.date))
  in
  let up_col, down_col, date_list =
    List.fold updown ~init:(0, 0, [])
      ~f:(fun (up, down, dates) (updown, date) ->
        match updown with
        | `UP -> (up + 1, down, date :: dates)
        | `DOWN -> (up, down + 1, date :: dates))
  in
  let dates_str =
    List.map date_list ~f:Date.to_string |> List.rev |> List.to_string ~f:ident
  in
  let up_ratio = float_of_int up_col /. float_of_int (up_col + down_col) in
  { code
  ; title = "oversold"
  ; column_message =
      [ { title = "up"; value = string_of_int up_col }
      ; { title = "down"; value = string_of_int down_col }
      ; { title = "up ratio"; value = string_of_float up_ratio }
      ; { title = "dates"; value = dates_str }
      ]
  }
