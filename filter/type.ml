open Core

module Attributed_data = struct
  type date = Date.t

  let date_to_yojson d = Date.to_string d |> [%to_yojson: string]

  let pp_date = Date.pp

  type t = {
    date: date [@to_yojson date_to_yojson];
    rsi_golden_cross: bool;
    rsi6_lt_40: bool;
    rsi6_lt_30: bool;
  }
  [@@deriving show, to_yojson]
end
