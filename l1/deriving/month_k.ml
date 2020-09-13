open Core
open Poly
open Option.Monad_infix

let month_k (data_list : L1_loader.Type.raw_data) : L1_loader.Type.raw_data =
  List.fold data_list ~init:(None, None, [])
    ~f:(fun (last_month, (month_data : L1_loader.Type.raw_data_elem option), r)
            current
            ->
      let month = Date.month (L1_loader.Type.date current) in
      match last_month with
      | None ->
        ( Some month
        , Some
            L1_loader.Type.
              { date = current.date
              ; opening = current.opening
              ; high = current.high
              ; low = current.low
              ; close = current.close
              ; ttm = current.ttm
              ; days = 1
              ; percent_change = 0.
              }
        , r )
      | Some last_m when Month.(month <> last_m) ->
        let percent_change =
          match r with
          | Some (last_month_data : L1_loader.Type.raw_data_elem) :: _ ->
            (current.close -. last_month_data.close) /. last_month_data.close
          | _ -> 0.
        in
        ( (* new month *)
          Some month
        , Some
            { date = current.date
            ; opening = current.opening
            ; high = current.high
            ; low = current.low
            ; close = current.close
            ; ttm = current.ttm
            ; days = 1
            ; percent_change
            }
        , month_data :: r )
      | Some _ ->
        (* continue with lastday *)
        ( Some month
        , ( month_data >>= fun month_data' ->
            let percent_change =
              match r with
              | Some last_month_data :: _ ->
                (current.close -. last_month_data.close)
                /. last_month_data.close
              | _ -> 0.
            in
            Some
              { month_data' with
                close = current.close
              ; high =
                  ( if current.high > month_data'.high then
                    current.high
                  else
                    month_data'.high )
              ; low =
                  ( if current.low < month_data'.low then
                    current.low
                  else
                    month_data'.low )
              ; ttm = current.ttm
              ; days = month_data'.days + 1
              ; percent_change
              } )
        , r ))
  |> fun (_, last_month, r) -> List.filter_opt (last_month :: r) |> List.rev

let%test "test-month_k" =
  let datal =
    L1_loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data)
      []
  in
  let month_data_list = month_k datal in
  List.for_all (L1_loader.Type.days_col month_data_list) ~f:(fun e -> e < 30)

let%test "test-month_k_percent_change" =
  let datal =
    L1_loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data)
      []
  in
  let month_data_list = month_k datal in
  let last = List.last_exn month_data_list in
  Float.round_decimal last.percent_change ~decimal_digits:4 = 0.0214
