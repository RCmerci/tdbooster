open Core

type raw_data =
  { date: Date.t
  (* ; time: Time.t (\* 日k:当天, 周k:周第一天, 月k:月第一天 *\) *)
  ; opening: float (* 开盘 *)
  ; high: float (* 最高 *)
  ; low: float (* 最低 *)
  ; closing: float
        (* 收盘 *)

        (* ; rose: float (\* 涨幅 *\)
   * ; amplitude: float (\* 振幅 *\)
   * ; total_hands: int64 (\* 总手 *\)
   * ; amount: int64 (\* 金额 *\)
   * ; exchange_hands: float (\* 换手% *\)
   * ; num_of_deal: int64 (\* 成交次数 *\) *)
  ; days: int (* 日k: 1, 周k: 本周交易天数, 月k: 本月交易天数 *)
  }
[@@deriving show]
