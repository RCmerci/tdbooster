open Core
open Option.Monad_infix

let week_k (data_list : Loader.Type.raw_data list) : Loader.Type.raw_data list
    =
  List.fold data_list ~init:(0, None, [])
    ~f:(fun (last_week_num, (week_data : Loader.Type.raw_data option), r)
       current
       ->
      let week_num =
        Date.week_number current.date
      in
      if last_week_num <> week_num then
        (* new week *)
        ( week_num
        , Some
            { date= current.date
            ; opening= current.opening
            ; closing= current.closing
            ; high= current.high
            ; low= current.low
            ; ttm= current.ttm
            ; days= 1 }
        , week_data :: r )
      else
        (* continue with lastday *)
        ( week_num
        , ( week_data
          >>= fun (week_data' : Loader.Type.raw_data) ->
          Some
            { week_data' with
              closing= current.closing
            ; high=
                ( if current.high > week_data'.high then current.high
                else week_data'.high )
            ; low=
                ( if current.low < week_data'.low then current.low
                  else week_data'.low )
            ; ttm= current.ttm
            ; days= week_data'.days + 1 } )
        , r ) )
  |> fun (_, last_week, r) -> List.filter_opt (last_week :: r) |> List.rev

let%test "test-week_k" =
  let datal =
    Loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data) []
  in
  let week_data_list = week_k datal in
  List.for_all week_data_list ~f:(fun e -> e.days < 6)
