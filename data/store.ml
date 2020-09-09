open Core

let db_open ~config_dir =
  Sqlite3.db_open (Filename.concat config_dir "tdbooster.db")

let all_codes ~custom_codes =
  List.concat [ custom_codes; Const.industry_code_list; Const.common_codes ]
  |> List.stable_dedup

let read_all_codes_raw_data ~config_dir ~custom_codes =
  List.map (all_codes ~custom_codes) ~f:(fun code ->
      match code with
      | c when List.mem Const.[ cl; gc; hg ] c ~equal:String.( = ) ->
        Loader.From_sina.Futures.read_from_file ~output_dir:config_dir ~code
      | _ ->
        Loader.From_txt.read_from_file
          (Filename.concat config_dir code)
          (Filename.concat config_dir (code ^ ".ttm")))
