module Date = struct
  include Core_kernel.Date

  let to_yojson (t : t) : Yojson.Safe.t = `String (to_string t)

  let of_yojson json =
    match json with
    | `String s -> (
      try Result.Ok (Core_kernel.Date.of_string s)
      with Invalid_argument e -> Result.Error e )
    | _ -> Result.Error "invalid_arg"
end

module Marketinfo_basedata_info = struct
  type attr =
    { date : Date.t
    ; ma_arranged : bool
    ; relative_strength : float (* price/zz800_price *)
    }
  [@@deriving yojson]

  type elem =
    { code : string
    ; industry : string option
    ; day_data : attr list
    }
  [@@deriving yojson]

  type t = elem list [@@deriving yojson]
end

module Marketinfo_industry_trend_info = struct
  (* t = (industry * (date * percent) list) list *)
  type t = (string * (Date.t * float) list) list [@@deriving yojson]
end

type search_info =
  { case : string
  ; codes : string list
  }
[@@deriving yojson]

(* (string * float) list: (date, num) list *)
type marketinfo =
  { title : string
  ; data : (Date.t * float) list
  }
[@@deriving yojson]

type output =
  { data : Marketinfo_basedata_info.t
  ; marketinfo : marketinfo list
  ; (* code date data *)
    industry_trend : Marketinfo_industry_trend_info.t
  ; search_info : search_info list
  }
[@@deriving yojson]
