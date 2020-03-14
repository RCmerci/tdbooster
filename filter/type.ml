open Core

module Attributed_data = struct
  type t = {
    date: Date.t;
    raw_data: Deriving.Type.Derived_data.t;
    rsi_golden_cross: bool; }
  [@@deriving show]
end
