open Core

module Derived_data = struct
  type t =
    { date : Date.t
    ; raw_data : L1_loader.Type.raw_data_elem [@opaque]
    ; ma20 : float
    ; ma60 : float
    ; ma120 : float
    ; ema12 : float
    ; ema20 : float
    ; ema26 : float
    ; ema60 : float
    ; ema120 : float
    ; dif : float
    ; dea : float
    ; macd : float
    ; bias24 : float
    ; rsi6 : float
    ; rsi12 : float
    ; rsi24 : float
    ; kdj933 : float * float * float
    ; rel5 : float
    ; rel20 : float
    ; rel60 : float
    ; rel120 : float
    }
  [@@deriving show]

  let date t = t.date

  let to_string = show
end
