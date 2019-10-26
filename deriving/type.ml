open Core

module Derived_data = struct
  type t =
    { date: Date.t
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

  let date t = t.date

  let to_string = show
end
