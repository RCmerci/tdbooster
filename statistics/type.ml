type column =
  { title : string
  ; value : string
  }
[@@deriving to_yojson]

type display_struct =
  { code : string
  ; title : string
  ; column_message : column list
  }
[@@deriving to_yojson]
