open Core

module Log = struct
  type t = string list

  let concat = List.join

  let to_string_list = List.rev
end

module Warning = struct
  type t = string list

  let concat = List.join

  let to_string_list = List.rev
end

module LogAndWarnWriter : sig
  type 'a m

  val log : string -> unit m

  val warn : string -> unit m

  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m

  val return : 'a -> 'a m

  val eval : 'a m -> Log.t * Warning.t * 'a
end = struct
  type 'a m = Log.t * Warning.t * 'a

  let log s = ([s], [], ())

  let warn s = ([], [s], ())

  let ( >>= ) m f =
    let l, w, a = m in
    let l', w', a' = f a in
    (Log.concat [l'; l], Warning.concat [w'; w], a')

  let return a = ([], [], a)

  let eval m = m
end
