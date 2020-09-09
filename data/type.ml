open Core

type base_data_elem =
  { date : Date.t
  ; opening : float
  ; high : float
  ; low : float
  ; close : float
  ; ttm : float
  ; days : int
  ; percent_change : float
  }

type derived_data_elem =
  { date : Date.t
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
  }

type industry_trend_data_elem =
  { date : Date.t
  ; above_ma20_percent : float
  }
