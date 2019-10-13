open Core

type raw_data =
  { time: Time.t
  ; opening: float
  ; high: float
  ; low: float
  ; closing: float
  ; rose: float
  ; amplitude: float
  ; total_hands: int64
  ; amount: int64
  ; exchange_hands: float
  ; num_of_deal: int64 }
[@@deriving show]
