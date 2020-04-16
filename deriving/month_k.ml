open Core
open Poly    
open Option.Monad_infix
open Owl
    
let month_k (data_list : Loader.Type.raw_data) : Loader.Type.raw_data
  =
  Loader.Type.fold data_list ~init:(None, None, [])
    ~f:(fun (last_month, (month_data :Dataframe.elt array option), r)
         current
         ->
           let month = Date.month (Loader.Type.date current) in
           match last_month with
           | None ->
             ( Some month
             , Some
                 Loader.Type.([|
                     (Dataframe.pack_string (date_string current));
                     (Dataframe.pack_float (opening current));
                     (Dataframe.pack_float (high current));
                     (Dataframe.pack_float (low current));
                     (Dataframe.pack_float (close current));
                     (Dataframe.pack_float (ttm current));
                     (Dataframe.pack_int 1);|])
             , r )
             
           | Some last_m when Month.(month <> last_m) ->
             ( (* new month *)
               Some month
             , Some
               Loader.Type.([|
                     (Dataframe.pack_string (date_string current));
                     (Dataframe.pack_float (opening current));
                     (Dataframe.pack_float (high current));
                     (Dataframe.pack_float (low current));
                     (Dataframe.pack_float (close current));
                     (Dataframe.pack_float (ttm current));
                     (Dataframe.pack_int 1);|])
             , month_data :: r )
           | Some _ ->
             (* continue with lastday *)
             ( Some month
             , ( month_data
                 >>= fun month_data' ->
                 let open Loader.Type in
                 set_close month_data' (close current);
                 set_high month_data' (
                   if (high current) > (high month_data') then
                     (high current)
                   else (high month_data'));
                 set_low month_data' (
                   if (low current) < (low month_data') then
                     (low current)
                   else (low month_data'));
                 set_ttm month_data' (ttm current);
                 set_days month_data' (days month_data' + 1);
                 Some month_data' )
             , r ) )
  |> fun (_, last_month, r) -> List.filter_opt (last_month :: r) |> List.rev |> Loader.Type.make_raw_data

let%test "test-month_k" =
  let datal =
    Loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data) []
  in
  let month_data_list = month_k datal in
  Array.for_all (Loader.Type.days_col month_data_list) ~f:(fun e -> e < 30)
