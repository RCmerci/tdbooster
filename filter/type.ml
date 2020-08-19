open Core

module Attributed_data = struct
  type date = Date.t

  let date_to_yojson d = Date.to_string d |> [%to_yojson: string]

  let pp_date = Date.pp

  type t = {
    date: date [@to_yojson date_to_yojson];
    industry: string option;
    rsi_golden_cross: bool;
    rsi6_lt_20: bool;
    rsi6_lt_30: bool;
    ma_up: bool;
    ma_arranged: bool;
    price_less_ma20: bool;
    relative_strength: float; (* price/zz800_price *)
    price_before_20: (float * string); (* (price, date) *)
    price_before_60: (float * string); (* (price, date) *)
    price_before_120: (float * string); (* (price, date) *)
  }
  [@@deriving show, to_yojson]
end

module Market_data = struct
  type t = {
    gc: (string * float) list;
    hg: (string * float) list;
    cl: (string * float) list;
    hg_div_gc: (string * float) list;
    cl_div_gc: (string * float) list;
  } [@@deriving show, to_yojson]
end
