open Core

module Selector = struct
  type t =
    | GE of Date.t
    | LE of Date.t
    | GT of Date.t
    | LT of Date.t
    | BetweenTE of (Date.t * Date.t)
    | BetweenEE of (Date.t * Date.t)
    | BetweenET of (Date.t * Date.t)
    | BetweenTT of (Date.t * Date.t)

  let to_where_clause t =
    "WHERE "
    ^
    match t with
    | GE d -> Printf.sprintf "date(date) >= date('%s')" (Date.to_string d)
    | GT d -> Printf.sprintf "date(date) > date('%s')" (Date.to_string d)
    | LE d -> Printf.sprintf "date(date) <= date('%s')" (Date.to_string d)
    | LT d -> Printf.sprintf "date(date) < date('%s')" (Date.to_string d)
    | BetweenTE (d1, d2) ->
      Printf.sprintf "date(date) > date('%s') and date(date) <= date('%s')"
        (Date.to_string d1) (Date.to_string d2)
    | BetweenTT (d1, d2) ->
      Printf.sprintf "date(date) > date('%s') and date(date) < date('%s')"
        (Date.to_string d1) (Date.to_string d2)
    | BetweenEE (d1, d2) ->
      Printf.sprintf "date(date) >= date('%s') and date(date) <= date('%s')"
        (Date.to_string d1) (Date.to_string d2)
    | BetweenET (d1, d2) ->
      Printf.sprintf "date(date) >= date('%s') and date(date) < date('%s')"
        (Date.to_string d1) (Date.to_string d2)
end

module type DataQuerySig = sig
  type t

  type r

  val create : ?db:Sqlite3.db -> config_dir:string -> t

  val query :
       t
    -> codes:string list
    -> dwm:[ `DAY | `WEEK | `MONTH ]
    -> selector:Selector.t
    -> r
end

module BaseData : DataQuerySig with type r = Type.base_data_map = struct
  type t = { db : Sqlite3.db }

  type r = Type.base_data_map

  let create ?db ~config_dir : t =
    let db' = Option.value db ~default:(Store.db_open ~config_dir) in
    { db = db' }

  let build_select ?selector code dwm =
    let where =
      Option.value_map selector ~default:"" ~f:Selector.to_where_clause
    in
    Printf.sprintf "SELECT * FROM %s %s;"
      (Store.BaseData.tablename ~dwm code)
      where

  let prepare_select db ?selector code dwm =
    build_select ?selector code dwm |> Sqlite3.prepare db

  let of_row (r : Sqlite3.Data.t array) : Type.base_data_elem option =
    let open Option.Monad_infix in
    let open Sqlite3.Data in
    to_string r.(0) >>= fun date' ->
    to_float r.(1) >>= fun opening ->
    to_float r.(2) >>= fun high ->
    to_float r.(3) >>= fun low ->
    to_float r.(4) >>= fun close ->
    let ttm = Option.value (to_float r.(5)) ~default:0. in
    let days = Option.value (to_int r.(6)) ~default:1 in
    let percent_change = Option.value (to_float r.(7)) ~default:0. in
    let date = Date.of_string date' in
    Some
      ( { date; opening; high; low; close; ttm; days; percent_change }
        : Type.base_data_elem )

  let query t ~codes ~dwm ~selector : Type.base_data_map =
    let f () =
      List.map codes ~f:(fun code ->
          let rc, elem_list =
            Sqlite3.fold (prepare_select t.db ~selector code dwm) ~init:[]
              ~f:(fun r row ->
                Option.value_map (of_row row) ~default:r ~f:(fun e -> e :: r))
          in
          Sqlite3.Rc.check rc;
          (code, Array.of_list elem_list))
    in
    Store.wrap_txn t.db f
    |> Map.of_alist_reduce (module String) ~f:(fun b1 _ -> b1)
end

module DerivedData : DataQuerySig with type r = Type.derived_data_map = struct
  type t = { db : Sqlite3.db }

  type r = Type.derived_data_map

  let create ?db ~config_dir : t =
    let db' = Option.value db ~default:(Store.db_open ~config_dir) in
    { db = db' }

  let build_select ?selector code dwm =
    let where =
      Option.value_map selector ~default:"" ~f:Selector.to_where_clause
    in
    Printf.sprintf "SELECT * FROM %s %s;"
      (Store.DerivedData.tablename ~dwm code)
      where

  let prepare_select db ?selector code dwm =
    build_select ?selector code dwm |> Sqlite3.prepare db

  let of_row (r : Sqlite3.Data.t array) : Type.derived_data_elem option =
    let open Option.Monad_infix in
    let open Sqlite3.Data in
    to_string r.(0) >>= fun date' ->
    to_float r.(1) >>= fun ma20 ->
    to_float r.(2) >>= fun ma60 ->
    to_float r.(3) >>= fun ma120 ->
    to_float r.(4) >>= fun ema12 ->
    to_float r.(5) >>= fun ema20 ->
    to_float r.(6) >>= fun ema26 ->
    to_float r.(7) >>= fun ema60 ->
    to_float r.(8) >>= fun ema120 ->
    to_float r.(9) >>= fun dif ->
    to_float r.(10) >>= fun dea ->
    to_float r.(11) >>= fun macd ->
    to_float r.(12) >>= fun bias24 ->
    to_float r.(13) >>= fun rsi6 ->
    to_float r.(14) >>= fun rsi12 ->
    to_float r.(15) >>= fun rsi24 ->
    to_float r.(16) >>= fun kdj933_1 ->
    to_float r.(17) >>= fun kdj933_2 ->
    to_float r.(18) >>= fun kdj933_3 ->
    let date = Date.of_string date' in
    Some
      ( { date
        ; ma20
        ; ma60
        ; ma120
        ; ema12
        ; ema20
        ; ema26
        ; ema60
        ; ema120
        ; dif
        ; dea
        ; macd
        ; bias24
        ; rsi6
        ; rsi12
        ; rsi24
        ; kdj933 = (kdj933_1, kdj933_2, kdj933_3)
        }
        : Type.derived_data_elem )

  let query t ~codes ~dwm ~selector : Type.derived_data_map =
    let f () =
      List.map codes ~f:(fun code ->
          let rc, elem_list =
            Sqlite3.fold (prepare_select t.db ~selector code dwm) ~init:[]
              ~f:(fun r row ->
                Option.value_map (of_row row) ~default:r ~f:(fun e -> e :: r))
          in
          Sqlite3.Rc.check rc;
          (code, Array.of_list elem_list))
    in
    Store.wrap_txn t.db f
    |> Map.of_alist_reduce (module String) ~f:(fun b _ -> b)
end

module IndustryTrendData = struct
  type t = { db : Sqlite3.db }

  type r = Type.industry_trend_data_map

  let create ?db ~config_dir =
    let db' = Option.value db ~default:(Store.db_open ~config_dir) in
    { db = db' }
end
