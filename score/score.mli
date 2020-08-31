type size

type t

val create_size : min:float -> max:float -> size

val create : min:float -> max:float -> float -> t

val max : size -> float

val min : size -> float

val of_float : size -> float -> t

val normalize : t -> float

val sum : t list -> t
