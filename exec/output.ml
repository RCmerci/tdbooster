open Core

type transinfo = {date: string; price: float; info: (string * string) list}
[@@deriving yojson]

type transaction =
  { i: transinfo [@key "I"]
  ; o: transinfo [@key "O"]
  ; logs: string list
  ; warnings: string list }
[@@deriving yojson]

type transactionlist = {name: string; trans: transaction list}
[@@deriving yojson]

type t = transactionlist list [@@deriving yojson]

let to_string t = Yojson.Safe.to_string (to_yojson t)
