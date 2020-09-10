open Core
open Sqlite3

let db_open ~config_dir =
  Sqlite3.db_open (Filename.concat config_dir "tdbooster.db")

let all_codes ~custom_codes =
  List.concat [ custom_codes; Const.industry_code_list; Const.common_codes ]
  |> List.stable_dedup

let read_all_codes_base_data ~config_dir ~custom_codes =
  List.map (all_codes ~custom_codes) ~f:(fun code ->
      Tuple2.create code
        ( match code with
        | c when List.mem Const.[ cl; gc; hg ] c ~equal:String.( = ) ->
          Loader.From_sina.Futures.read_from_file ~output_dir:config_dir ~code
        | _ ->
          Loader.From_txt.read_from_file
            (Filename.concat config_dir code)
            (Filename.concat config_dir (code ^ ".ttm")) ))

let base_data_table_name ~dwm code =
  let dwm' =
    match dwm with
    | `DAY -> "day"
    | `WEEK -> "week"
    | `MONTH -> "month"
  in
  "base_" ^ dwm' ^ "_" ^ String.substr_replace_all code ~pattern:"." ~with_:"_"

let base_data_table_drop db ~dwm code =
    let tablename = base_data_table_name ~dwm code in
    exec db ("DROP TABLE" ^ tablename)

let base_data_table_create db ~dwm code =
  let tablename = base_data_table_name ~dwm code in
  exec db
    ("CREATE TABLE " ^
     tablename ^ " (" ^
     "date TEXT PRIMARY KEY NOT NULL," ^
     "opening FLOAT NULL," ^
     "high FLOAT NULL," ^
     "low FLOAT NULL," ^
     "close FLOAT NULL," ^
     "ttm FLOAT NULL," ^
     "days INT NULL," ^
     "percent_change FLOAT NULL" ^
     ");")[@ocamlformat "disable"] |> ignore;
  exec db ("CREATE UNIQUE INDEX index_date on " ^ tablename ^ "(date);") |> ignore



let base_data_elem_insert_sql ~dwm code =
  "INSERT INTO "
  ^ base_data_table_name ~dwm code
  ^ "(date, opening, high, low, close, ttm, days, percent_change)"
  ^ "VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

let base_data_to_db db ~dwm code raw_data_list =
  let insert_stmt = prepare db (base_data_elem_insert_sql ~dwm code) in
  exec db "begin;" |> Rc.check;
  List.iter raw_data_list ~f:(fun (e : Loader.Type.raw_data_elem) ->
      reset insert_stmt |> Rc.check;
      bind insert_stmt 1 (Sqlite3.Data.TEXT (Date.to_string e.date)) |> Rc.check;
      bind insert_stmt 2 (Sqlite3.Data.FLOAT e.opening) |> Rc.check;
      bind insert_stmt 3 (Sqlite3.Data.FLOAT e.high) |> Rc.check;
      bind insert_stmt 4 (Sqlite3.Data.FLOAT e.low) |> Rc.check;
      bind insert_stmt 5 (Sqlite3.Data.FLOAT e.close) |> Rc.check;
      bind insert_stmt 6 (Sqlite3.Data.FLOAT e.ttm) |> Rc.check;
      bind insert_stmt 7 (Sqlite3.Data.INT (Int64.of_int e.days)) |> Rc.check;
      bind insert_stmt 8 (Sqlite3.Data.FLOAT e.percent_change) |> Rc.check;
      step insert_stmt |> Rc.check);
  exec db "commit;" |> Rc.check

let store_base_data db ~dwm codes_and_raw_data_list =
  List.iter codes_and_raw_data_list ~f:(fun (code, raw_data_list) ->
      (* create table if not exists, so ignore error here *)
      ignore (base_data_table_create db ~dwm code);
      base_data_to_db db ~dwm code raw_data_list)
