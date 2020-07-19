module Log :
  sig
    type t
    val concat : t list -> t
    val to_string_list : t -> string list
  end
module Warning :
  sig
    type t
    val concat : t list -> t
    val to_string_list : t -> string list
  end
module LogAndWarnWriter :
  sig
    type 'a m
    val log : string -> unit m
    val logf: ('a -> string, unit, string) format -> 'a -> unit m
    val warn : string -> unit m
    val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
    val return : 'a -> 'a m
    val eval : 'a m -> Log.t * Warning.t * 'a
  end
