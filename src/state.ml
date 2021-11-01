open Board
open Player
open Bag
open Graphics

type bonus =
  | Letter of int
  | Word of int  (** Represents the score bonuses for scrabble. *)

type key = {
  color : (int * bonus) list;
  letter : (string * int) list;
}
(** Represents the scoring key for scrabble. *)

type event =
  | Play
  | Tile_clicked of int
      (** Represents a description of the current state of the game. *)

type t = {
  event : event;
  (* description of the current state of the game. *)
  key : key;
  (* scrabble scoring key *)
  board : Board.b;
  (* scrabble board *)
  bag : Bag.b;
  (* bag of unused tiles *)
  players : Player.t list;
  (* list of players *)
  turn : Player.t; (* player whose turn it is currently *)
}

let draw_tile (state : t) (p : Player.t) : t = state

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
  let players = [ Player.init "1" ] in
  {
    event = Play;
    board = Board.init ();
    players;
    bag = Bag.init ();
    key = init_key ();
    turn = List.hd players;
  }

(**[color_key color phrase x y w h] draws a rectangle with lower left
   corner at ([x],[y]) with width [w] and height [h] and colors it with
   [color]. [phrase] is displayed to the right of the rectangle*)
let color_key color phrase x y w h =
  set_color color;
  draw_rect x y w h;
  fill_rect x y w h;
  moveto (x + w) (y + (h / 2));
  set_color black;
  draw_string phrase

(**[letter_key letter number] prints a string with lower left corner at
   ([x],[y]) that associates [letter] with [number]*)
let letter_key letter number x y =
  moveto x y;
  draw_string (Printf.sprintf "%s - %s" letter number)

let draw_key () =
  draw_rect 30 30 140 565;
  moveto 50 575;
  draw_string "Key";
  color_key 0xFF0000 " -3x Word" 50 540 25 25;
  color_key 0x0000FF " -3x Letter" 50 510 25 25;
  color_key 0xE88282 " -2x Word" 50 480 25 25;
  color_key 0x8282E8 " -2x Letter" 50 450 25 25;
  letter_key "A" "1" 50 430;
  letter_key "B" "3" 50 415;
  letter_key "C" "3" 50 400;
  letter_key "D" "2" 50 385;
  letter_key "E" "1" 50 370;
  letter_key "F" "4" 50 355;
  letter_key "G" "2" 50 340;
  letter_key "H" "4" 50 325;
  letter_key "I" "1" 50 310;
  letter_key "J" "8" 50 295;
  letter_key "K" "5" 50 280;
  letter_key "L" "1" 50 265;
  letter_key "M" "3" 50 250;
  letter_key "N" "1" 50 235;
  letter_key "Q" "10" 50 220;
  letter_key "R" "1" 50 205;
  letter_key "U" "1" 50 190;
  letter_key "V" "4" 50 175;
  letter_key "O" "1" 50 160;
  letter_key "P" "3" 50 145;
  letter_key "S" "1" 50 130;
  letter_key "T" "1" 50 115;
  letter_key "W" "4" 50 100;
  letter_key "X" "8" 50 85;
  letter_key "Y" "4" 50 70;
  letter_key "Z" "10" 50 55

(**[player_boxes l h num] draws [num] player boxes depending on the
   length [l] and height [h] of the entire board. Requires: [l], [h] >
   0. 2 <= [num] <= 4.*)
let rec player_boxes l h num =
  if num = 0 then ()
  else
    let x = 208 in
    let y = (100 * (4 - num)) + 180 in
    draw_rect x y 160 90;
    moveto (x + 16) (y + 72);
    draw_string ("Player " ^ string_of_int num);
    if num > 0 then player_boxes l h (num - 1)

let init_draw t =
  Board.draw t.board;
  Player.init_draw ();
  Bag.init_draw ();
  draw_key ();
  player_boxes 800 625 4

let click x y state =
  match state.event with
  | Play ->
      if Bag.clicked x y then
        if Player.num_tiles state.turn < 7 then
          let letter = Bag.find_letter state.bag in
          {
            state with
            turn = Player.add_tile state.turn letter;
            bag = Bag.remove state.bag letter;
          }
        else state
      else if Player.clicked state.turn x y then
        { state with event = Tile_clicked x }
      else state
  | Tile_clicked x0 ->
      if Board.clicked x y state.board then
        {
          state with
          event = Play;
          turn = Player.remove_tile state.turn x0;
          board = Board.add_tile x y "A" state.board;
        }
      else { state with event = Play }

let game_over state = false

let draw (state : t) : unit =
  Player.draw state.turn;
  Bag.draw state.bag;
  Board.draw state.board;
  moveto 0 0;
  match state.event with
  | Play -> ()
  | Tile_clicked x0 -> draw_string (string_of_int x0)
