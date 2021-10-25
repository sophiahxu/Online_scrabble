type b
(**The type of the board.*)

type t
(**The type of a tile.*)

val board_setup : int -> int -> b
(**[board_setup x y] is the board with total length [x] and height [y].*)

val side : b -> int 
(**[side x] is the length of a single tile on the board.*)

val tiles : b -> t list 
(**[tiles x] is the list of tiles on the board.*)

val name : t -> string
(**[name a] is the name of tile [a].*)

val tile_x : t -> int 
(**[tile_x a] is the x coordinate of the tile [a].*)

val tile_y : t -> int 
(**[tile_y a] is the y coordinate of the tile [a].*)

val color : t -> int option
(**[color a] is the color of the tile [a]*)

val letter : t -> char 
(**[letter a] is the letter on tile [a].*)
