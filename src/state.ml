open Board
open Players

type bag_tile = string * int

type bag = {
  count : int;
  tiles : bag_tile list;
}

type info = {
  board : Board.b;
  players : Players.player list;
  bag : bag;
}

type t =
  | Loading_initial
  | Initial
  | Loading_active
  | Active of info
  | Loading_complete
  | Complete

(** [find_letter_helper tiles n] is the [n]th letter in [tiles] starting
    at "A". Requires: [n] is less than or equal to the number of tiles
    in [tiles].*)
let rec find_letter_helper (tiles : bag_tile list) (n : int) : string =
  match tiles with
  | [] -> raise (Failure "Invalid n argument.")
  | (l, c) :: t -> if n <= c then l else find_letter_helper t (n - c)

(** [find_letter b] is a random letter in [b].*)
let find_letter (b : bag) : string =
  Random.self_init ();
  let n = Random.int b.count in
  find_letter_helper b.tiles n

(** [remove tiles letter] removes [letter] from [tiles]. Requires:
    [letter] must exist in a quantity of at least 1 in [tiles], and
    [letter] is a capital letter. *)
let rec remove (tiles : bag_tile list) (letter : string) : bag_tile list
    =
  match tiles with
  | [] -> []
  | (l, n) :: t ->
      if l = letter then (l, n - 1) :: remove t letter
      else (l, n) :: remove t letter

let draw (i : info) (p : player) : unit =
  raise (Failure "unimplemented")

let bag_count (i : info) : int = i.bag.count

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

let init_info () =
  {
    board = board_setup 0 0;
    players = [ init_player "1" ];
    bag = init_bag ();
  }
