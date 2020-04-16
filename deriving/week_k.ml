open Core
open Poly    
open Option.Monad_infix
open Owl
    
let week_k (data_list : Loader.Type.raw_data) : Loader.Type.raw_data 
    =
    Loader.Type.fold data_list ~init:(0, None, [])
    ~f:(fun (last_week_num, (week_data : Dataframe.elt array option), r)
       current
       ->
      let week_num =
        Date.week_number (Loader.Type.date current)
      in
      if last_week_num <> week_num then
        (* new week *)
        ( week_num
        , Some
          Loader.Type.([|
                     (Dataframe.pack_string (date_string current));
                     (Dataframe.pack_float (opening current));
                     (Dataframe.pack_float (high current));
                     (Dataframe.pack_float (low current));
                     (Dataframe.pack_float (close current));
                     (Dataframe.pack_float (ttm current));
                     (Dataframe.pack_int 1);|])
        , week_data :: r )
      else
        (* continue with lastday *)
        ( week_num
        , ( week_data
            >>= fun (week_data' : Dataframe.elt array) ->
            let open Loader.Type in
            set_close week_data' (close current);
            set_high week_data' (if (high current) > (high week_data')
                                 then (high current)
                                 else (high week_data'));
            set_low week_data' (if (low current) < (low week_data')
                                then (low current)
                                else (low week_data'));
            set_ttm week_data' (ttm current);
            set_days week_data' (days week_data'+1);
            Some week_data')
        , r ) )
  |> fun (_, last_week, r) -> List.filter_opt (last_week :: r) |> List.rev |> Loader.Type.make_raw_data

let%test "test-week_k" =
  let datal =
    Loader.From_txt.read_from_string_lines
      (String.split_lines Testdata.Data.data) []
  in
  let week_data_list = week_k datal in
  Array.for_all (Loader.Type.days_col week_data_list) ~f:(fun e -> e < 6)
