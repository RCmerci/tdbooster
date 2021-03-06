open Core
open Poly
open Option.Monad_infix

let week_k (data_list : L1_loader.Type.raw_data) : L1_loader.Type.raw_data =
  List.fold data_list ~init:(0, None, [])
    ~f:(fun (last_week_num, (week_data : L1_loader.Type.raw_data_elem option), r)
            current
            ->
      let week_num = Date.week_number (L1_loader.Type.date current) in
      if last_week_num <> week_num then
        (* new week *)
        let percent_change =
          match r with
          | Some (last_week_data : L1_loader.Type.raw_data_elem) :: _ ->
            (current.close -. last_week_data.close) /. last_week_data.close
          | _ -> 0.
        in
        ( week_num
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
        , week_data :: r )
      else
        (* continue with lastday *)
        let percent_change =
          match r with
          | Some last_week_data :: _ ->
            (current.close -. last_week_data.close) /. last_week_data.close
          | _ -> 0.
        in
        ( week_num
        , ( week_data >>= fun (week_data' : L1_loader.Type.raw_data_elem) ->
            Some
              { week_data' with
                close = current.close
              ; high =
                  ( if current.high > week_data'.high then
                    current.high
                  else
                    week_data'.high )
              ; low =
                  ( if current.low < week_data'.low then
                    current.low
                  else
                    week_data'.low )
              ; ttm = current.ttm
              ; days = week_data'.days + 1
              ; percent_change
              } )
        , r ))
  |> fun (_, last_week, r) -> List.filter_opt (last_week :: r) |> List.rev

let%test "test-week_k" =
  let datal =
    L1_loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data)
      []
  in
  let week_data_list = week_k datal in
  List.for_all (L1_loader.Type.days_col week_data_list) ~f:(fun e -> e < 6)

let%test "test-week_k_percent_change" =
  let datal =
    L1_loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data)
      []
  in
  let week_data_list = week_k datal in
  let last = List.last_exn week_data_list in
  Float.round_decimal last.percent_change ~decimal_digits:4 = 0.0214
