open Graphics
open Yojson.Basic.Util

type bag_tile = string * int
(** represents a tile in the bag *)

type t = {
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

let find_letter (bag : t) : string =
  Random.self_init ();
  let n = Random.int bag.count + 1 in
  (*n is in [1..b.count]*)
  find_letter_helper bag.tiles n

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

let remove (bag : t) (letter : string) : t =
  { count = bag.count - 1; tiles = remove_helper bag.tiles letter }

let count (bag : t) : int = bag.count

let count_letter bag letter = List.assoc letter bag.tiles

let init () =
  {
    count = 98;
    tiles =
      Yojson.Basic.from_file "data/bag.json"
      |> to_assoc
      |> List.map (fun (k, v) -> (k, v |> to_int));
  }

let clicked x y =
  x > 222 && x < 348
  && float_of_int y
     < (((60. ** 2.) -. ((float_of_int x -. 288.) ** 2.)) ** 0.5) +. 90.
  && float_of_int y
     > -.(((60. ** 2.) -. ((float_of_int x -. 288.) ** 2.)) ** 0.5)
       +. 90.

let draw bag =
  set_color 0x8282E8;
  fill_circle 288 90 60;
  set_color black;
  draw_circle 288 90 60;
  moveto 276 92;
  draw_string "DRAW";
  moveto 252 74;
  draw_string (string_of_int bag.count ^ " tiles left")
