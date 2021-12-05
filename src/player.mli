type t
(**Represents a player*)

val init : string -> t
(**[init s] initializes a player with name [s], who starts with zero
   points and seven empty tiles*)

val add_points : int -> t -> t
(**[add_points pt p] adds [pt] points to player [p]*)

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
(**[draw_tiles p] draws the tile holder and the tiles of [p] at each
   tiles specified location*)

val remove_tile : t -> int -> t
(**[remove_tile p l] removes the tile at location [l] inside [p]'s tiles*)

val clicked : t -> int -> int -> bool
(**[clicked p x y] returns true if the tile at location [(x,y)] is
   non-empty inside player [p], and returns false if the tile is empty*)

val letter : t -> int -> string
(**[letter p l] returns the letter at location [l] inside player [p]'s
   tiles*)

val update_player : t -> t list -> t list
(**[update_player player player_list] is [player_list] with the correct
   player in the list updated according to [player]. Requires: [player]
   must have the same name as exactly one player in [player_list].*)

val next_turn : t -> t list -> t
(**[next_turn player player_list] is the next player in [player_list].
   Requires: [player] must have the same name as exactly one player in
   [player_list], and [player_list] has length of at least 2.*)

val undo : t -> t
(**[undo player] is [player] with it's latest tile placement undone. If
   the [player] has not made any moves yet this turn, no changes are
   made.*)

val undo_all : t -> t
(**[undo_all player] is [player] with all of it's tile placements
   undone. If the [player] has not made any moves yet this turn, no
   changes are made*)

val clear_mem : t -> t
(**[clear_mem player] will clear [player]'s memory at the start of a
   turn, so that player has no tiles placed on the board for that turn*)

val get_skip : t -> bool
(**[skip player] returns true if a player should be skipped, false if
   not*)

val change_skip : t -> t
(**[change_skip player] makes [player] skippable if not already, and not
   skippable if they already were*)

val draw_box : t -> bool -> unit
(**[draw_box player status] draws the name, box and points of [player]
   in blue if [status] is true, and draws in black if [status] is false*)
