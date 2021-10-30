open Graphics

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

let count (bag : b) : int = bag.count

let count_letter bag letter = List.assoc letter bag.tiles

let init () =
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

let init_draw bag =
  draw_circle 288 90 60;
  moveto 276 92;
  draw_string "DRAW";
  moveto 252 74;
  draw_string "98 tiles left"

let clicked x y =
  x > 222 && x < 348
  && float_of_int y
     < (((60. ** 2.) -. ((float_of_int x -. 288.) ** 2.)) ** 0.5) +. 90.
  && float_of_int y
     > -.(((60. ** 2.) -. ((float_of_int x -. 288.) ** 2.)) ** 0.5)
       +. 90.

let draw bag =
  set_color white;
  fill_circle 288 90 60;
  set_color black;
  draw_circle 288 90 60;
  moveto 272 90;
  draw_string "DRAW";
  moveto 248 72;
  draw_string (string_of_int bag.count ^ " tiles left")
