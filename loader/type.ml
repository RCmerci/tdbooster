open Core

(* date: 日k:当天, 周k:周第一天, 月k:月第一天
 * days: 日k: 1, 周k: 本周交易天数, 月k: 本月交易天数
 * [|"date";"open";"high";"low";"close";"ttm";"days"|] *)
type raw_data_elem =
  { date : Date.t
  ; opening : float
  ; high : float
  ; low : float
  ; close : float
  ; ttm : float
  ; days : int
  ; percent_change : float
  }

type raw_data = raw_data_elem list

let date_col t = List.map t ~f:(fun e -> e.date)

let opening_col t = List.map t ~f:(fun e -> e.opening)

let high_col t = List.map t ~f:(fun e -> e.high)

let low_col t = List.map t ~f:(fun e -> e.low)

let close_col t = List.map t ~f:(fun e -> e.close)

let ttm_col t = List.map t ~f:(fun e -> e.ttm)

let days_col t = List.map t ~f:(fun e -> e.days)

let percent_change t = List.map t ~f:(fun e -> e.percent_change)

let date_string e = Date.to_string e.date

let date e = e.date

let opening e = e.opening

let high e = e.high

let low e = e.low

let close e = e.close

let ttm e = e.ttm

let days e = e.days

module RawData = struct
  type t = raw_data_elem

  let date t = t.date

  let to_string _ = "<unimplemented>"
end

module IndustryList = struct
  type one =
    { category : string
    ; codes : string list
    }
  [@@deriving show]

  type t = one list [@@deriving show]
end
