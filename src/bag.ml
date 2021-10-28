type bag_tile = string * int
(** represents a tile in the bag *)

type b = {
  count : int;
  tiles : bag_tile list;
}

(** [find_letter_helper tiles n] is the [n]th letter in [tiles] starting
    at "A". Requires: [n] is less than or equal to the number of tiles
    in [tiles] and it is greater than 0.*)
let rec find_letter_helper (tiles : bag_tile list) (n : int) : string =
  match tiles with
  | [] -> raise (Failure "Invalid n argument.")
  | (l, c) :: t -> if n <= c then l else find_letter_helper t (n - c)

let find_letter (b : b) : string =
  Random.self_init ();
  let n = Random.int b.count + 1 in
  (*n is in [1..b.count]*)
  find_letter_helper b.tiles n

(** [remove_helper tiles letter] removes [letter] from [tiles].
    Requires: [letter] must exist in a quantity of at least 1 in
    [tiles], and [letter] is a capital letter. *)
let rec remove_helper (tiles : bag_tile list) (letter : string) :
    bag_tile list =
  match tiles with
  | [] -> []
  | (l, n) :: t ->
      if l = letter then (l, n - 1) :: remove_helper t letter
      else (l, n) :: remove_helper t letter

let remove (bag : b) (letter : string) : b =
  { count = bag.count - 1; tiles = remove_helper bag.tiles letter }

let bag_count (bag : b) : int = bag.count

let init_bag () =
  {
    count = 98;
    tiles =
      [
        ("A", 9);
        ("B", 2);
        ("C", 2);
        ("D", 4);
        ("E", 12);
        ("F", 2);
        ("G", 3);
        ("H", 2);
        ("I", 9);
        ("J", 1);
        ("K", 1);
        ("L", 4);
        ("M", 2);
        ("N", 6);
        ("O", 8);
        ("P", 2);
        ("Q", 1);
        ("R", 6);
        ("S", 4);
        ("T", 6);
        ("U", 4);
        ("V", 2);
        ("W", 2);
        ("X", 1);
        ("Y", 2);
        ("Z", 1);
      ];
  }