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

let dwm_to_string dwm =
  match dwm with
  | `DAY -> "day"
  | `WEEK -> "week"
  | `MONTH -> "month"

let wrap_txn (type a) db (f : unit -> a) : a =
  Sqlite3.(
    ignore (exec db "begin;");
    let r = f () in
    ignore (exec db "commit;");
    r)

module BaseData = struct
  let tablename ~dwm code =
    "base_" ^ dwm_to_string dwm ^ "_"
    ^ String.substr_replace_all code ~pattern:"." ~with_:"_"

  let table_drop db ~dwm code =
    let tablename = tablename ~dwm code in
    exec db ("DROP TABLE " ^ tablename)

  let table_create db ~dwm code =
    let tablename = tablename ~dwm code in
    ignore
      (exec db
         ( "CREATE TABLE " ^ tablename ^ " ("
         ^ "date TEXT PRIMARY KEY NOT NULL," ^ "opening FLOAT NULL,"
         ^ "high FLOAT NULL," ^ "low FLOAT NULL," ^ "close FLOAT NULL,"
         ^ "ttm FLOAT NULL," ^ "days INT NULL," ^ "percent_change FLOAT NULL"
         ^ ");" ));
    ignore
      (exec db ("CREATE UNIQUE INDEX index_date on " ^ tablename ^ "(date);"))

  let insert_sql ~dwm code =
    "INSERT INTO " ^ tablename ~dwm code
    ^ "(date, opening, high, low, close, ttm, days, percent_change)"
    ^ "VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

  let to_db db ~dwm code raw_data_list =
    let insert_stmt = prepare db (insert_sql ~dwm code) in
    let f () =
      List.iter raw_data_list ~f:(fun (e : Loader.Type.raw_data_elem) ->
          reset insert_stmt |> Rc.check;
          bind insert_stmt 1 (Sqlite3.Data.TEXT (Date.to_string e.date))
          |> Rc.check;
          bind insert_stmt 2 (Sqlite3.Data.FLOAT e.opening) |> Rc.check;
          bind insert_stmt 3 (Sqlite3.Data.FLOAT e.high) |> Rc.check;
          bind insert_stmt 4 (Sqlite3.Data.FLOAT e.low) |> Rc.check;
          bind insert_stmt 5 (Sqlite3.Data.FLOAT e.close) |> Rc.check;
          bind insert_stmt 6 (Sqlite3.Data.FLOAT e.ttm) |> Rc.check;
          bind insert_stmt 7 (Sqlite3.Data.INT (Int64.of_int e.days))
          |> Rc.check;
          bind insert_stmt 8 (Sqlite3.Data.FLOAT e.percent_change) |> Rc.check;
          step insert_stmt |> Rc.check)
    in
    wrap_txn db f

  let store db ~dwm codes_and_raw_data_list =
    List.iter codes_and_raw_data_list ~f:(fun (code, raw_data_list) ->
        (* create table if not exists, so ignore error here *)
        ignore (table_create db ~dwm code);
        to_db db ~dwm code raw_data_list)

  let store_day_data db codes_and_raw_data_list =
    store db ~dwm:`DAY codes_and_raw_data_list

  let store_week_data db codes_and_raw_data_list =
    let codes_and_raw_week_data_list =
      List.map codes_and_raw_data_list ~f:(fun (code, raw_data_list) ->
          (code, Deriving.Week_k.week_k raw_data_list))
    in
    store db ~dwm:`WEEK codes_and_raw_week_data_list

  let store_month_data db codes_and_raw_data_list =
    let codes_and_raw_week_data_list =
      List.map codes_and_raw_data_list ~f:(fun (code, raw_data_list) ->
          (code, Deriving.Month_k.month_k raw_data_list))
    in
    store db ~dwm:`MONTH codes_and_raw_week_data_list
end

module DerivedData = struct
  let tablename ~dwm code =
    "derived_" ^ dwm_to_string dwm ^ "_"
    ^ String.substr_replace_all code ~pattern:"." ~with_:"_"

  let table_drop db ~dwm code =
    let tablename = tablename ~dwm code in
    exec db ("DROP TABLE" ^ tablename)

  let table_create db ~dwm code =
    let tablename = tablename ~dwm code in
    ignore
      (exec db
         ( "CREATE TABLE " ^ tablename ^ " ("
         ^ "date TEXT PRIMARY KEY NOT NULL," ^ "ma20 FLOAT NULL,"
         ^ "ma60 FLOAT NULL," ^ "ma120 FLOAT NULL," ^ "ema12 FLOAT NULL,"
         ^ "ema20 FLOAT NULL," ^ "ema26 INT NULL," ^ "ema60 FLOAT NULL,"
         ^ "ema120 FLOAT NULL," ^ "dif FLOAT NULL," ^ "dea FLOAT NULL,"
         ^ "macd FLOAT NULL," ^ "bias24 FLOAT NULL," ^ "rsi6 FLOAT NULL,"
         ^ "rsi12 FLOAT NULL," ^ "rsi24 FLOAT NULL," ^ "kjd933_1 FLOAT NULL,"
         ^ "kjd933_2 FLOAT NULL," ^ "kjd933_3 FLOAT NULL" ^ ");" ));
    ignore
      (exec db ("CREATE UNIQUE INDEX index_date on " ^ tablename ^ "(date);"))

  let insert_sql ~dwm code =
    "INSERT INTO " ^ tablename ~dwm code
    ^ "(date, ma20, ma60, ma120, ema12, ema20, ema26, ema60, ema120, dif, dea, \
       macd, bias24, rsi6, rsi12, rsi24, kdj933_1, kdj933_2, kdj933_3)"
    ^ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

  let to_db db ~dwm code (derived_data_list : Deriving.Type.Derived_data.t list)
      =
    let insert_stmt = prepare db (insert_sql ~dwm code) in
    let f () =
      List.iter derived_data_list ~f:(fun e ->
          reset insert_stmt |> Rc.check;
          bind insert_stmt 1 (Sqlite3.Data.TEXT (Date.to_string e.date))
          |> Rc.check;
          bind insert_stmt 2 (Sqlite3.Data.FLOAT e.ma20) |> Rc.check;
          bind insert_stmt 3 (Sqlite3.Data.FLOAT e.ma60) |> Rc.check;
          bind insert_stmt 4 (Sqlite3.Data.FLOAT e.ma120) |> Rc.check;
          bind insert_stmt 5 (Sqlite3.Data.FLOAT e.ema12) |> Rc.check;
          bind insert_stmt 6 (Sqlite3.Data.FLOAT e.ema20) |> Rc.check;
          bind insert_stmt 7 (Sqlite3.Data.FLOAT e.ema26) |> Rc.check;
          bind insert_stmt 8 (Sqlite3.Data.FLOAT e.ema60) |> Rc.check;
          bind insert_stmt 9 (Sqlite3.Data.FLOAT e.ema120) |> Rc.check;
          bind insert_stmt 10 (Sqlite3.Data.FLOAT e.dif) |> Rc.check;
          bind insert_stmt 11 (Sqlite3.Data.FLOAT e.dea) |> Rc.check;
          bind insert_stmt 12 (Sqlite3.Data.FLOAT e.macd) |> Rc.check;
          bind insert_stmt 13 (Sqlite3.Data.FLOAT e.bias24) |> Rc.check;
          bind insert_stmt 14 (Sqlite3.Data.FLOAT e.rsi6) |> Rc.check;
          bind insert_stmt 15 (Sqlite3.Data.FLOAT e.rsi12) |> Rc.check;
          bind insert_stmt 16 (Sqlite3.Data.FLOAT e.rsi24) |> Rc.check;
          bind insert_stmt 17 (Sqlite3.Data.FLOAT (Tuple3.get1 e.kdj933))
          |> Rc.check;
          bind insert_stmt 18 (Sqlite3.Data.FLOAT (Tuple3.get2 e.kdj933))
          |> Rc.check;
          bind insert_stmt 19 (Sqlite3.Data.FLOAT (Tuple3.get3 e.kdj933))
          |> Rc.check;
          step insert_stmt |> Rc.check)
    in
    wrap_txn db f

  let raw_data_to_day_derived_data raw_data_list =
    Option.value (Deriving.Unify.unify_day raw_data_list) ~default:[]

  let raw_data_to_week_derived_data raw_data_list =
    Option.value (Deriving.Unify.unify_week raw_data_list) ~default:[]

  let raw_data_to_month_derived_data raw_data_list =
    Option.value (Deriving.Unify.unify_month raw_data_list) ~default:[]

  let store db ~dwm codes_and_derived_data_list =
    List.iter codes_and_derived_data_list ~f:(fun (code, derived_data_list) ->
        ignore (table_create db ~dwm code);
        to_db db ~dwm code derived_data_list)

  let store_day_data db codes_and_raw_data_list =
    let codes_and_derived_data_list =
      List.map codes_and_raw_data_list ~f:(fun (code, raw_data_list) ->
          (code, raw_data_to_day_derived_data raw_data_list))
    in
    store db ~dwm:`DAY codes_and_derived_data_list

  let store_week_data db codes_and_raw_data_list =
    let codes_and_derived_data_list =
      List.map codes_and_raw_data_list ~f:(fun (code, raw_data_list) ->
          (code, raw_data_to_week_derived_data raw_data_list))
    in
    store db ~dwm:`WEEK codes_and_derived_data_list

  let store_month_data db codes_and_raw_data_list =
    let codes_and_derived_data_list =
      List.map codes_and_raw_data_list ~f:(fun (code, raw_data_list) ->
          (code, raw_data_to_month_derived_data raw_data_list))
    in
    store db ~dwm:`MONTH codes_and_derived_data_list
end

module IndustryTrendData = struct
  let tablename industry = "industry_trend_" ^ industry

  let table_drop db industry = exec db ("DROP TABLE " ^ tablename industry)

  let table_create db industry =
    ignore
      (exec db
         ( "CREATE TABLE " ^ tablename industry ^ " ("
         ^ "date TEXT PRIMARY KEY NOT NULL," ^ "above_ma20_percent FLOAT NULL"
         ^ ");" ));
    ignore
      (exec db
         ("CREATE UNIQUE INDEX index_date on " ^ tablename industry ^ "(date);"))

  let insert_sql industry =
    "INSERT INTO " ^ tablename industry ^ "(date, above_ma20_percent)"
    ^ "VALUES (?, ?)"

  let to_db db industry (data_list : (Date.t * Score.t) list) =
    let insert_stmt = prepare db (insert_sql industry) in
    let f () =
      List.iter data_list ~f:(fun (date, score) ->
          reset insert_stmt |> Rc.check;
          bind insert_stmt 1 (Sqlite3.Data.TEXT (Date.to_string date))
          |> Rc.check;
          bind insert_stmt 2 (Sqlite3.Data.FLOAT (Score.normalize score))
          |> Rc.check;
          step insert_stmt |> Rc.check)
    in
    wrap_txn db f

  let store db industry_and_data_list =
    List.iter industry_and_data_list ~f:(fun (industry, data_list) ->
        (* create table if not exists, so ignore error here *)
        ignore (table_create db industry);
        to_db db industry data_list)
end
