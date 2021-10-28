type b
(**[b] represents the bag of unused tiles*)

val find_letter : b -> string
(** [find_letter b] is a random letter in [b].*)

val remove : b -> string -> b
(** [remove bag letter] removes [letter] from [bag]. Requires: [letter]
    must exist in a quantity of at least 1 in [bag], and [letter] is a
    capital letter. *)

val bag_count : b -> int
(** [bag_count bag] is the number of tiles in [bag]. *)

val init_bag : unit -> b
(** [init_bag] is the initial scrabble bag filled with 98 tiles
    according to official scrabble rules (minus the two blank tiles). *)
