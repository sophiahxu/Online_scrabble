type t
(**[t] represents the bag of unused tiles*)

val find_letter : t -> string
(** [find_letter bag] is a random letter in [bag].*)

val remove : t -> string -> t
(** [remove bag letter] removes [letter] from [bag]. Requires: [letter]
    must exist in a quantity of at least 1 in [bag], and [letter] is a
    capital letter. *)

val count : t -> int
(** [count bag] is the number of tiles in [bag]. *)

val count_letter : t -> string -> int
(** [count_letter bag letter] is the number of tiles in [bag] that are
    [letter]. Requires: [letter] is a capital letter. *)

val clicked : int -> int -> bool
(** [clicked x y] is true if the the coordinates [x],[y] are in the
    "draw" button. *)

val init : unit -> t
(** [init] is the initial scrabble bag filled with 98 tiles according to
    official scrabble rules (minus the two blank tiles). *)

val draw : t -> unit
(** [draw bag] draws "draw" button according to the [bag]. *)
