type t
(**[info] represents the scrabble game state*)

val init : unit -> t
(**[init ()] initializes the scrabble game state for the start of the
   game*)

val init_draw : t -> unit
(**[init_draw state] draws the initial state of the game.*)

val draw_tile : t -> Players.t -> t
(** [draw_tile state player] adds a random tile from the bag to the
    [player]'s tiles and removes that tile from bag if [player] has <= 7
    tiles.*)

val click : int -> int -> t -> t
(**[click x y state] is an updated [state] depending on the location [x]
   and [y] of where the mouse clicked.*)

val draw : t -> unit
(**[draw state] draws all of the current scrabble game components in
   [state] to the GUI.*)

val game_over : t -> bool
(**[game_over state] is true with the game is over, else it is false.*)
