type b

type t

val board_setup : int -> int -> b

val side : b -> int

val tiles : b -> t list

val tile_x : t -> int

val tile_y : t -> int

val color : t -> int option

val letter : t -> char

val name : t -> string