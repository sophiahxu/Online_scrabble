type b
(**The type of the board.*)

type t
(**The type of a tile.*)

val init : unit -> b
(**[init ()] is the board with all its values initialized.*)

val draw : b -> unit
(**[init_draw board] draws the original board [board].*)

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
(**[color a] is the color of the tile [a].*)

val letter : t -> string
(**[letter a] is the letter on tile [a].*)

val turn : t -> bool
(**[turn a] is whether or not the tile was placed on the current turn.*)

val memory_stack : b -> t list
(**[memory_stack a] is the list of tiles that were placed on the current
   turn for board [b]*)

val clicked : int -> int -> b -> bool
(**[clicked x y] is whether or not the dimension *)

val add_tile : int -> int -> string -> b -> b
(**[add_tile x y b l] is the new board after replacing the tile at x, y
   with one that has the letter [l]. Requires: x, y are coordinates of a
   valid tile to replace (i.e. one that does not already have a letter
   on it and is on the board)*)

val undo : b -> b
(**[undo board] is [board] with the latest tile placement undone. If
   there have been no new placements yet this turn, no changes are made.*)

val undo_all : b -> b
(**[undo_all board] is [board] with all the tiles placed during the
   current player's turn removed. If there have been no new placements
   yet this turn, no changes are made. *)

val clear_mem : b -> b
(**[clearn_mem board] is [board] with an empty memory stack.*)

val score : b -> int
(**[score board] tallies up the score according to [board] for this
   turn.*)
