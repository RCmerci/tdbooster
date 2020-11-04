open Std

module Condition = struct
  type index =
    | Ma20 of float
    | Ma60 of float
    | Ma120 of float
    | Ema12 of float
    | Ema20 of float
    | Ema26 of float
    | Ema60 of float
    | Ema120 of float
    | Dif of float
    | Dea of float
    | Macd of float
    | Bias24 of float
    | Rsi6 of float
    | Rsi12 of float
    | Rsi24 of float
    | Kdj933_1 of float
    | Kdj933_2 of float
    | Kdj933_3 of float
    | Rel5 of float
    | Rel20 of float
    | Rel60 of float
    | Rel120 of float

  type dateSelector =
    | DateGE of Date.t
    | DateLE of Date.t
    | DateGT of Date.t
    | DateLT of Date.t
    | DateBetweenTE of (Date.t * Date.t)
    | DateBetweenEE of (Date.t * Date.t)
    | DateBetweenET of (Date.t * Date.t)
    | DateBetweenTT of (Date.t * Date.t)

  type simple = private Simple_t_not_used

  type combine = private Combine_t_not_used

  type 'a t =
    | AND : simple t list -> combine t
    | LT : (dateSelector * index) -> simple t
    | LE : (dateSelector * index) -> simple t
    | GT : (dateSelector * index) -> simple t
    | GE : (dateSelector * index) -> simple t

  let index_to_name_string i =
    match i with
    | Ma20 _ -> "ma20"
    | Ma60 _ -> "ma60"
    | Ma120 _ -> "ma120"
    | Ema12 _ -> "ema12"
    | Ema20 _ -> "ema20"
    | Ema26 _ -> "ema26"
    | Ema60 _ -> "ema60"
    | Ema120 _ -> "ema120"
    | Dif _ -> "dif"
    | Dea _ -> "dea"
    | Macd _ -> "macd"
    | Bias24 _ -> "bias24"
    | Rsi6 _ -> "rsi6"
    | Rsi12 _ -> "rsi12"
    | Rsi24 _ -> "rsi24"
    | Kdj933_1 _ -> "kdj933_1"
    | Kdj933_2 _ -> "kdj933_2"
    | Kdj933_3 _ -> "kdj933_3"
    | Rel5 _ -> "rel5"
    | Rel20 _ -> "rel20"
    | Rel60 _ -> "rel60"
    | Rel120 _ -> "rel120"

  let index_value i =
    match i with
    | Ma20 v -> v
    | Ma60 v -> v
    | Ma120 v -> v
    | Ema12 v -> v
    | Ema20 v -> v
    | Ema26 v -> v
    | Ema60 v -> v
    | Ema120 v -> v
    | Dif v -> v
    | Dea v -> v
    | Macd v -> v
    | Bias24 v -> v
    | Rsi6 v -> v
    | Rsi12 v -> v
    | Rsi24 v -> v
    | Kdj933_1 v -> v
    | Kdj933_2 v -> v
    | Kdj933_3 v -> v
    | Rel5 v -> v
    | Rel20 v -> v
    | Rel60 v -> v
    | Rel120 v -> v

  let dateSelector_to_string d =
    match d with
    | DateGE date ->
      Printf.sprintf "date(date) >= date('%s')" (Date.to_string date)
    | DateLE date ->
      Printf.sprintf "date(date) <= date('%s')" (Date.to_string date)
    | DateGT date ->
      Printf.sprintf "date(date) > date('%s')" (Date.to_string date)
    | DateLT date ->
      Printf.sprintf "date(date) < date('%s')" (Date.to_string date)
    | DateBetweenTE (date1, date2) ->
      Printf.sprintf "date(date) > date('%s') and date(date) <= date('%s')"
        (Date.to_string date1) (Date.to_string date2)
    | DateBetweenEE (date1, date2) ->
      Printf.sprintf "date(date) >= date('%s') and date(date) <= date('%s')"
        (Date.to_string date1) (Date.to_string date2)
    | DateBetweenET (date1, date2) ->
      Printf.sprintf "date(date) >= date('%s') and date(date) < date('%s')"
        (Date.to_string date1) (Date.to_string date2)
    | DateBetweenTT (date1, date2) ->
      Printf.sprintf "date(date) > date('%s') and date(date) < date('%s')"
        (Date.to_string date1) (Date.to_string date2)

  (* TODO: optimize output-sql *)
  let rec to_clause_aux : type a. a t -> string =
   fun t ->
    match t with
    | AND ts ->
      List.map ts ~f:(fun t -> to_clause_aux t) |> String.concat ~sep:" and "
    | LT (d, i) ->
      let date_clause = dateSelector_to_string d in
      Printf.sprintf "%s and %s < %f" date_clause (index_to_name_string i)
        (index_value i)
    | LE (d, i) ->
      let date_clause = dateSelector_to_string d in
      Printf.sprintf "%s and %s <= %f" date_clause (index_to_name_string i)
        (index_value i)
    | GT (d, i) ->
      let date_clause = dateSelector_to_string d in
      Printf.sprintf "%s and %s > %f" date_clause (index_to_name_string i)
        (index_value i)
    | GE (d, i) ->
      let date_clause = dateSelector_to_string d in
      Printf.sprintf "%s and %s >= %f" date_clause (index_to_name_string i)
        (index_value i)

  let to_clause : type a. a t -> string = fun t -> "WHERE " ^ to_clause_aux t
end

(* TODO: add table all_codes to reduce ~custom_codes param *)
let all_codes = Store.all_codes

type t =
  { db : Sqlite3.db
  ; dwm : [ `DAY | `WEEK | `MONTH ]
  ; custom_codes : string list
  }

let create ~config_dir ~dwm ~custom_codes =
  let db = Store.db_open ~config_dir in
  { db; dwm; custom_codes }

(** use [codes] instead of all_codes in db if [codes] provided *)
let search t ?codes cond =
  let where_clause = Condition.to_clause cond in
  let allcodes =
    Option.value codes ~default:(all_codes ~custom_codes:t.custom_codes)
  in
  List.filter allcodes ~f:(fun code ->
      let count_sql =
        Printf.sprintf "SELECT COUNT(date) FROM %s %s;"
          (Store.DerivedData.tablename ~dwm:t.dwm code)
          where_clause
      in
      let found = ref false in
      Sqlite3.exec_not_null_no_headers t.db count_sql ~cb:(fun count ->
          if int_of_string count.(0) > 0 then found := true)
      |> Sqlite3.Rc.check;
      !found)
