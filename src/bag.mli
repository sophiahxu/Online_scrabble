type b
(**[b] represents the bag of unused tiles*)

val find_letter : b -> string
(** [find_letter b] is a random letter in [b].*)

val remove : b -> string -> b
(** [remove bag letter] removes [letter] from [bag]. Requires: [letter]
    must exist in a quantity of at least 1 in [bag], and [letter] is a
    capital letter. *)

val count : b -> int
(** [count bag] is the number of tiles in [bag]. *)

val count_letter : b -> string -> int
(** [count_letter bag letter] is the number of tiles in [bag] that are
    [letter]. Requires: [letter] is a capital letter. *)

val clicked : int -> int -> bool
(** [clicked x y] is true if the the coordinates [x],[y] are in the
    "draw" button. *)

val init : unit -> b
(** [init] is the initial scrabble bag filled with 98 tiles according to
    official scrabble rules (minus the two blank tiles). *)

val init_draw : b -> unit
(**[init bag] draws the initial bag button.*)

val draw : b -> unit
(** [draw bag] draws the updated "draw" button according to the new
    [bag]. *)
