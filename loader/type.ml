open Core

type raw_data =
  { date: Date.t
  (* ; time: Time.t (\* 日k:当天, 周k:周第一天, 月k:月第一天 *\) *)
  ; opening: float (* 开盘 *)
  ; high: float (* 最高 *)
  ; low: float (* 最低 *)
  ; closing: float (* 收盘 *)
  ; ttm: float option     (* 滚动市盈率 *)
  ; days: int (* 日k: 1, 周k: 本周交易天数, 月k: 本月交易天数 *)
  }
[@@deriving show]
