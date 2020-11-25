open Std
open Sqlite3

type raw_data_list = L1.Loader.Type.raw_data

let db_delete ~config_dir =
  try Caml_unix.unlink (Filename.concat config_dir "tdbooster.db")
  with _ -> ()

let db_open ~config_dir =
  Sqlite3.db_open (Filename.concat config_dir "tdbooster.db")

let all_codes ~custom_codes =
  List.concat
    [ custom_codes; Const.industry_code_list; Const.common_codes; Const.hs300 ]
  |> List.stable_dedup

module ReadCodesBaseData : sig
  type codes_and_raw_data_list

  type codes_and_raw_week_data_list

  type codes_and_raw_month_data_list

  val read_all_codes_base_data :
    config_dir:string -> custom_codes:string list -> codes_and_raw_data_list

  val fetch_all_codes_base_data :
    config_dir:string -> custom_codes:string list -> unit

  val read_all_codes_week_base_data :
    codes_and_raw_data_list -> codes_and_raw_week_data_list

  val read_all_codes_month_base_data :
    codes_and_raw_data_list -> codes_and_raw_month_data_list

  val unpack :
    codes_and_raw_data_list -> (string * L1.Loader.Type.raw_data) list

  val pack : (string * L1.Loader.Type.raw_data) list -> codes_and_raw_data_list

  val unpack_week :
    codes_and_raw_week_data_list -> (string * L1.Loader.Type.raw_data) list

  val pack_week :
    (string * L1.Loader.Type.raw_data) list -> codes_and_raw_week_data_list

  val unpack_month :
    codes_and_raw_month_data_list -> (string * L1.Loader.Type.raw_data) list

  val pack_month :
    (string * L1.Loader.Type.raw_data) list -> codes_and_raw_month_data_list
end = struct
  type codes_and_raw_data_list = (string * L1.Loader.Type.raw_data) list

  type codes_and_raw_week_data_list = codes_and_raw_data_list

  type codes_and_raw_month_data_list = codes_and_raw_data_list

  let unpack = ident

  let pack = ident

  let unpack_week = ident

  let pack_week = ident

  let unpack_month = ident

  let pack_month = ident

  let refresh_data_aux codes output_dir =
    let module T = Lwt_throttle.Make (Unit) in
    let throttler = T.create ~rate:3 ~max:99999 ~n:1 in
    let open Lwt.Infix in
    let (_ : unit list) =
      Lwt_list.map_p
        (fun code ->
          let get_data =
            match code with
            | "GC"
            | "HG"
            | "CL" ->
              L1.Loader.From_sina.Futures.get_futrues_data
            | _ ->
              fun ~output_dir ~code ->
                L1.Loader.From_baostock.run_py_script ~code ~output_dir
                >>= fun _ ->
                L1.Loader.From_baostock_ttm.run_py_script ~code ~output_dir
                >>= fun _ -> Lwt.return_unit
          in
          T.wait throttler () >>= fun _ -> get_data ~code ~output_dir)
        codes
      |> Lwt_main.run
    in
    ()

  let read_all_codes_base_data ~config_dir ~custom_codes :
      codes_and_raw_data_list =
    List.map (all_codes ~custom_codes) ~f:(fun code ->
        Tuple2.create code
          ( match code with
          | c when List.mem Const.[ cl; gc; hg ] c ~equal:String.( = ) ->
            L1.Loader.From_sina.Futures.read_from_file ~output_dir:config_dir
              ~code
          | _ ->
            L1.Loader.From_txt.read_from_file
              (Filename.concat config_dir code)
              (Filename.concat config_dir (code ^ ".ttm")) ))

  let fetch_all_codes_base_data ~config_dir ~custom_codes =
    let rec refresh_errcodes data err_codes count =
      if count = 0 then
        failwithf "fetchdata failed: %s" (List.to_string err_codes ~f:ident) ()
      else
        let err_codes = Validate_data.BaseData.validate data in
        match err_codes with
        | [] -> ()
        | _ ->
          refresh_data_aux err_codes config_dir;
          let data = read_all_codes_base_data ~config_dir ~custom_codes in
          refresh_errcodes data err_codes (count - 1)
    in
    refresh_data_aux (all_codes ~custom_codes) config_dir;
    let data = read_all_codes_base_data ~config_dir ~custom_codes in
    refresh_errcodes data [] 3

  let read_all_codes_week_base_data codes_and_raw_data_list =
    List.map codes_and_raw_data_list ~f:(fun (code, raw_data_list) ->
        (code, L1.Deriving.Week_k.week_k raw_data_list))

  let read_all_codes_month_base_data codes_and_raw_data_list =
    List.map codes_and_raw_data_list ~f:(fun (code, raw_data_list) ->
        (code, L1.Deriving.Month_k.month_k raw_data_list))
end

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
      List.iter raw_data_list ~f:(fun (e : L1.Loader.Type.raw_data_elem) ->
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

  open ReadCodesBaseData

  let store db ~dwm (codes_and_raw_data_list : codes_and_raw_data_list) =
    List.iter (unpack codes_and_raw_data_list) ~f:(fun (code, raw_data_list) ->
        (* create table if not exists, so ignore error here *)
        ignore (table_create db ~dwm code);
        to_db db ~dwm code raw_data_list)

  let store_day_data db codes_and_raw_data_list =
    store db ~dwm:`DAY codes_and_raw_data_list

  let store_week_data db codes_and_raw_data_list =
    store db ~dwm:`WEEK
      (pack
         (unpack_week (read_all_codes_week_base_data codes_and_raw_data_list)))

  let store_month_data db codes_and_raw_data_list =
    store db ~dwm:`MONTH
      (pack
         (unpack_month (read_all_codes_month_base_data codes_and_raw_data_list)))

  let to_day_base_data raw_data_list = raw_data_list

  let to_week_base_data raw_data_list = L1.Deriving.Week_k.week_k raw_data_list

  let to_month_base_data raw_data_list =
    L1.Deriving.Month_k.month_k raw_data_list

  let to_day_derived_data raw_data_list zz800_raw_data_list =
    Option.value
      (L1.Deriving.Unify.unify_day raw_data_list zz800_raw_data_list)
      ~default:[]

  let to_week_derived_data raw_data_list zz800_raw_data_list =
    Option.value
      (L1.Deriving.Unify.unify_week raw_data_list zz800_raw_data_list)
      ~default:[]

  let to_month_derived_data raw_data_list zz800_raw_data_list =
    Option.value
      (L1.Deriving.Unify.unify_month raw_data_list zz800_raw_data_list)
      ~default:[]
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
         ^ "rsi12 FLOAT NULL," ^ "rsi24 FLOAT NULL," ^ "kdj933_1 FLOAT NULL,"
         ^ "kdj933_2 FLOAT NULL," ^ "kdj933_3 FLOAT NULL," ^ "rel5 INT NULL,"
         ^ "rel20 INT NULL," ^ "rel60 INT NULL," ^ "rel120 INT NULL" ^ ");" ));
    ignore
      (exec db ("CREATE UNIQUE INDEX index_date on " ^ tablename ^ "(date);"))

  let insert_sql ~dwm code =
    "INSERT INTO " ^ tablename ~dwm code
    ^ "(date, ma20, ma60, ma120, ema12, ema20, ema26, ema60, ema120, dif, dea, \
       macd, bias24, rsi6, rsi12, rsi24, kdj933_1, kdj933_2, kdj933_3, rel5, \
       rel20, rel60, rel120)"
    ^ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, \
       ?, ?)"

  let to_db db ~dwm code
      (derived_data_list : L1.Deriving.Type.Derived_data.t list) =
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
          bind insert_stmt 20 (Sqlite3.Data.INT (Int64.of_int e.rel5))
          |> Rc.check;
          bind insert_stmt 21 (Sqlite3.Data.INT (Int64.of_int e.rel20))
          |> Rc.check;
          bind insert_stmt 22 (Sqlite3.Data.INT (Int64.of_int e.rel60))
          |> Rc.check;
          bind insert_stmt 23 (Sqlite3.Data.INT (Int64.of_int e.rel120))
          |> Rc.check;
          step insert_stmt |> Rc.check)
    in
    wrap_txn db f

  open ReadCodesBaseData

  let get_zz800_raw_data_list codes_and_raw_data_list =
    List.Assoc.find_exn
      (unpack codes_and_raw_data_list)
      ~equal:( = ) Const.zz800

  let store db ~dwm codes_and_derived_data_list =
    List.iter codes_and_derived_data_list ~f:(fun (code, derived_data_list) ->
        ignore (table_create db ~dwm code);
        to_db db ~dwm code derived_data_list)

  let store_day_data db codes_and_raw_data_list =
    let codes_and_derived_data_list =
      List.map (unpack codes_and_raw_data_list) ~f:(fun (code, raw_data_list) ->
          try
            ( code
            , BaseData.to_day_derived_data raw_data_list
                (get_zz800_raw_data_list codes_and_raw_data_list) )
          with e -> failwithf "%s: %s" code (Exn.to_string e) ())
    in
    store db ~dwm:`DAY codes_and_derived_data_list

  let store_week_data db codes_and_raw_data_list =
    let codes_and_derived_data_list =
      List.map (unpack codes_and_raw_data_list) ~f:(fun (code, raw_data_list) ->
          ( code
          , BaseData.to_week_derived_data raw_data_list
              (get_zz800_raw_data_list codes_and_raw_data_list) ))
    in
    store db ~dwm:`WEEK codes_and_derived_data_list

  let store_month_data db codes_and_raw_data_list =
    let codes_and_derived_data_list =
      List.map (unpack codes_and_raw_data_list) ~f:(fun (code, raw_data_list) ->
          ( code
          , BaseData.to_month_derived_data raw_data_list
              (get_zz800_raw_data_list codes_and_raw_data_list) ))
    in
    store db ~dwm:`MONTH codes_and_derived_data_list

  let to_cursormap codes_and_raw_data_list =
    let codes_and_derived_data_list =
      List.map (unpack codes_and_raw_data_list) ~f:(fun (code, raw_data_list) ->
          ( code
          , BaseData.to_day_derived_data raw_data_list
              (get_zz800_raw_data_list codes_and_raw_data_list) ))
    in
    let module C = L1.Cursor.Data_cursor in
    Map.of_alist_reduce
      (module String)
      codes_and_derived_data_list
      ~f:(fun b _ -> b)
    |> Map.map ~f:C.create_exn
end

module IndustryTrendData = struct
  let tablename ~dwm industry =
    "industry_trend_" ^ dwm_to_string dwm ^ "_" ^ industry

  let table_drop db ~dwm industry =
    exec db ("DROP TABLE " ^ tablename ~dwm industry)

  let table_create db ~dwm industry =
    ignore
      (exec db
         ( "CREATE TABLE " ^ tablename ~dwm industry ^ " ("
         ^ "date TEXT PRIMARY KEY NOT NULL," ^ "above_ma20_percent FLOAT NULL"
         ^ ");" ));
    ignore
      (exec db
         ( "CREATE UNIQUE INDEX index_date on " ^ tablename ~dwm industry
         ^ "(date);" ))

  let insert_sql ~dwm industry =
    "INSERT INTO " ^ tablename ~dwm industry ^ "(date, above_ma20_percent)"
    ^ "VALUES (?, ?)"

  let to_db db ~dwm industry (data_list : (Date.t * Score.t) list) =
    let insert_stmt = prepare db (insert_sql ~dwm industry) in
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

  open ReadCodesBaseData

  let store db ~dwm industry_and_data_list =
    List.iter industry_and_data_list ~f:(fun (industry, data_list) ->
        (* create table if not exists, so ignore error here *)
        ignore (table_create db ~dwm industry);
        to_db db ~dwm industry data_list)

  let store_day_data db codes_and_raw_data_list =
    let cm = DerivedData.to_cursormap codes_and_raw_data_list in
    let industry_and_data_list =
      L1.Filter.Industry_trend.above_ma20_trend
        ~auxf:L1.Filter.Industry_trend.above_ma20_trend_all_aux cm
    in
    store db ~dwm:`DAY industry_and_data_list

  let store_week_data db codes_and_raw_data_list =
    let codes_and_raw_week_data_list =
      List.map (unpack codes_and_raw_data_list) ~f:(fun (code, v) ->
          (code, BaseData.to_week_base_data v))
    in
    let cm = DerivedData.to_cursormap (pack codes_and_raw_week_data_list) in
    let industry_and_data_list =
      L1.Filter.Industry_trend.above_ma20_trend
        ~auxf:L1.Filter.Industry_trend.above_ma20_trend_all_aux cm
    in
    store db ~dwm:`WEEK industry_and_data_list

  let store_month_data db codes_and_raw_data_list =
    let codes_and_raw_month_data_list =
      List.map (unpack codes_and_raw_data_list) ~f:(fun (code, v) ->
          (code, BaseData.to_month_base_data v))
    in
    let cm = DerivedData.to_cursormap (pack codes_and_raw_month_data_list) in
    let industry_and_data_list =
      L1.Filter.Industry_trend.above_ma20_trend
        ~auxf:L1.Filter.Industry_trend.above_ma20_trend_all_aux cm
    in
    store db ~dwm:`MONTH industry_and_data_list
end
