type t
(**Represents a player*)

val init : string -> t
(**[init s] initializes a player with name [s], who starts with zero
   points and seven empty tiles*)

val init_draw : unit -> unit
(**[init_draw ()] draws the initial tiles for [p].*)

val add_points : t -> int -> t
(**[add_points p pt] adds [pt] points to player [p]*)

val player_name : t -> string
(**[player_name p] returns the name of player [p]*)

val player_points : t -> int
(**[player_points p] returns the current points of player [p]*)

val num_tiles : t -> int
(**[num_tiles p] is the number of tiles in player [p]'s hand.*)

val add_tile : t -> string -> t
(**[add_tile p s] adds tile with letter [s] to player [p]'s next
   available tile location*)

val draw : t -> unit
(**[draw_tiles p] draws the tiles of [p] at each tiles specified
   location*)

val remove_tile : t -> int -> t
(**[remove_tile p l] removes the tile at location [l] inside [p]'s tiles*)

val clicked : t -> int -> int -> bool
(**[clicked p x y] returns true if the tile at location [(x,y)] is
   non-empty inside player [p], and returns false if the tile is empty*)