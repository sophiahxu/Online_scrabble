type t
(**[info] represents the scrabble game state*)

val init : unit -> t
(**[init ()] initializes the scrabble game state for the start of the
   game*)

val draw_tile : t -> Players.player -> t
(** [draw_tile state player] adds a random tile from the bag to the
    [player]'s tiles and removes that tile from bag if [player] has <= 7
    tiles.*)
