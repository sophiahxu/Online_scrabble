type player_tile
(**Represents a player's tile*)

type player
(**Represents a player*)

val init_player : string -> player
(*[init_player s] initializes a player with name [s], who starts with
  zero points and seven empty tiles*)

val add_points : player -> int -> player
(**[add_points p pt] adds [pt] points to player [p]*)

val player_name : player -> string
(**[player_name p] returns the name of player [p]*)

val player_points : player -> int
(**[player_points p] returns the current points of player [p]*)

val player_tiles : player -> player_tile list
(**[player_tiles p] returns the current tiles of player [p]*)

val num_tiles : player -> int
(**[num_tiles p] is the number of tiles in player [p]'s hand.*)

val make_tile : string -> int * int -> int -> player_tile
(**[make_tile l lo s] makes a tile representing letter [l], with the
   tile placed at location [lo], and a tile length of [s]*)

val add_tile : player -> player_tile -> player
(**[add_tile p pt] adds tile [pt] to player [p]'s current tiles.
   Requires: player [p] currently has not more than 7 tiles*)

val draw_tiles : player -> unit
(**[draw_tiles p] draws the tiles of [p] at each tiles specified
   location*)
