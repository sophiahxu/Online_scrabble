open Board
open Players
open Bag

(** Represents the score bonuses for scrabble. *)
type bonus =
  | Letter of int
  | Word of int

type k = {
  color : (int * bonus) list;
  letter : (string * int) list;
}
(** Represents the scoring key for scrabble. *)

type t = {
  key : k;
  (* scrabble scoring key *)
  board : Board.b;
  (* scrabble board *)
  bag : Bag.b;
  (* bag of unused tiles *)
  players : Players.player list;
  (* list of players *)
  turn : player; (* player whose turn it is currently *)
}

let draw_tile (state : t) (p : player) : t = state

(** [init_key () is the scrabble scoring key] *)
let init_key () =
  {
    color =
      [
        (0xFF0000, Word 3);
        (0x0000FF, Letter 3);
        (0xE88282, Word 2);
        (0x8282E8, Letter 2);
      ];
    letter =
      [
        ("A", 1);
        ("B", 3);
        ("C", 3);
        ("D", 3);
        ("E", 1);
        ("F", 4);
        ("G", 2);
        ("H", 4);
        ("I", 1);
        ("J", 8);
        ("K", 5);
        ("L", 1);
        ("M", 3);
        ("N", 1);
        ("O", 1);
        ("P", 3);
        ("Q", 10);
        ("R", 1);
        ("S", 1);
        ("T", 1);
        ("U", 1);
        ("V", 4);
        ("W", 4);
        ("X", 8);
        ("Y", 4);
        ("Z", 10);
      ];
  }

let init () =
  let players = [ init_player "1" ] in
  {
    board = board_setup 800 600;
    players;
    bag = Bag.init ();
    key = init_key ();
    turn = List.hd players;
  }

let click x y state =
  if Bag.clicked x y then
    if Players.num_tiles state.turn < 7 then
      let letter = Bag.find_letter state.bag in
      {
        state with
        turn = Players.add_tile state.turn letter;
        bag = Bag.remove state.bag letter;
      }
    else state
  else state

let game_over state = false

let draw (state : t) =
  Players.draw_tiles state.turn;
  Bag.draw state.bag
