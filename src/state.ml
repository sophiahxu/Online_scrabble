open Board
open Players
open Bag

exception NotFoundError

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

(**[nth lst n] is the element of [lst] with index [n]. Raises:
   NotFoundError if [n] >= List.length lst.*)
let rec nth lst n =
  match lst with
  | [] -> raise NotFoundError
  | a :: _ when n = 0 -> a
  | _ :: b -> nth b (n - 1)

let init () =
  let players = [ init_player "1" ] in
  {
    board = Board.init ();
    players;
    bag = Bag.init ();
    key = init_key ();
    turn = List.hd players;
  }

let init_draw t =
  Board.init_draw t.board;
  Players.init_draw (nth t.players 0);
  Bag.init_draw t.bag
