open Core
open Option.Monad_infix

let week_k (data_list : Loader.Type.raw_data list) : Loader.Type.raw_data list
    =
  List.fold data_list ~init:(8, None, [])
    ~f:(fun (last_day_of_week, (week_data : Loader.Type.raw_data option), r)
       current
       ->
      assert (Option.is_some current.day_of_week) ;
      let day_of_week = Option.value_exn current.day_of_week in
      if last_day_of_week >= day_of_week then
        (* new week *)
        ( day_of_week
        , Some
            { time= current.time
            ; day_of_week= None
            ; opening= current.opening
            ; closing= current.closing
            ; high= current.high
            ; low= current.low
            ; rose= 0.
            ; amplitude= 0.
            ; total_hands= 0L
            ; amount= 0L
            ; exchange_hands= 0.
            ; num_of_deal= 0L
            ; days= 1 }
        , week_data :: r )
      else
        (* continue with lastday *)
        ( day_of_week
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
            ; days= week_data'.days + 1 } )
        , r ) )
  |> fun (_, last_week, r) -> List.filter_opt (last_week :: r) |> List.rev

let%test "test-week_k" =
  let datal =
    Loader.From_tonghuashun_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data)
  in
  let week_data_list = week_k datal in
  let week_data_500 = List.nth_exn week_data_list 500 in
  week_data_500.opening = 111.1
  && week_data_500.closing = 105.06
  && week_data_500.high = 111.11
  && week_data_500.low = 99.43
