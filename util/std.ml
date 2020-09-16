include Core
include Poly

module Date = struct
  include Core.Date

  (* Add Yojson Deriving support *)

  let to_yojson (t : t) : Yojson.Safe.t = `String (to_string t)

  let of_yojson json =
    match json with
    | `String s -> (
      try Result.Ok (Date.of_string s)
      with Invalid_argument e -> Result.Error e )
    | _ -> Result.Error "invalid_arg"

  (* TODO: add utop printer *)
end
