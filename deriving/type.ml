open Core

module Derived_data = struct
  type t =
    { time: Time.t
    ; raw_data: Loader.Type.raw_data
    ; ema5: float
    ; ema10: float
    ; ema12: float
    ; ema14: float
    ; ema26: float
    ; ema60: float
    ; dif: float
    ; dea: float
    ; macd: float }
  [@@deriving show]

  let date t = Date.of_time t.time ~zone:(Time.Zone.of_utc_offset ~hours:8)
end
