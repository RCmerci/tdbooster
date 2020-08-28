open Core

type transinfo =
  { date : string
  ; price : float
  ; info : (string * string) list
  }
[@@deriving yojson]

type transaction =
  { i : transinfo [@key "I"]
  ; o : transinfo option [@key "O"]
  ; logs : string list
  ; warnings : string list
  }
[@@deriving yojson]

type attention =
  { logs : string list
  ; warnings : string list
  }
[@@deriving yojson]

type transactionlist =
  { name : string
  ; trans : transaction list
  ; attentions : attention list
  }
[@@deriving yojson]

type t = transactionlist list [@@deriving yojson]

let to_string t = Yojson.Safe.to_string (to_yojson t)
