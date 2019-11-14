open Core
open Option.Monad_infix

let month_k (data_list : Loader.Type.raw_data list) : Loader.Type.raw_data list
  =
  List.fold data_list ~init:(None, None, [])
    ~f:(fun (last_month, (month_data : Loader.Type.raw_data option), r)
         current
         ->
           let month = Date.month current.date
           in
           match last_month with
           | None ->
             ( Some month
             , Some
                 { date= current.date
                 ; opening= current.opening
                 ; closing= current.closing
                 ; high= current.high
                 ; low= current.low
                 ; ttm=None
                 ; days= 1 }
             , r )
           | Some last_m when Month.(month <> last_m) ->
             ( (* new month *)
               Some month
             , Some
                 { date= current.date
                 ; opening= current.opening
                 ; closing= current.closing
                 ; high= current.high
                 ; low= current.low
                 ; ttm=None
                 ; days= 1 }
             , month_data :: r )
           | Some _ ->
             (* continue with lastday *)
             ( Some month
             , ( month_data
                 >>= fun month_data' ->
                 Some
                   { month_data' with
                     closing= current.closing
                   ; high=
                       ( if current.high > month_data'.high then current.high
                         else month_data'.high )
                   ; low=
                       ( if current.low < month_data'.low then current.low
                         else month_data'.low )
                   ; days= month_data'.days + 1 } )
             , r ) )
  |> fun (_, last_month, r) -> List.filter_opt (last_month :: r) |> List.rev

let%test "test-month_k" =
  let datal =
    Loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data) []
  in
  let month_data_list = month_k datal in
  List.for_all month_data_list ~f:(fun e -> e.days < 30)
