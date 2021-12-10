open Graphics
open Yojson.Basic.Util

exception NotFoundError

type input =
  | Clicked of int * int
  | Enter
  | Z

(** Represents the input mode that the game is in. *)
type mode =
  | Draw
  | Click
  | Point of int
  | Init_challenge
  | Challenge
  | End_challenge

type t = {
  mode : mode;
  (* description of the current state of the game. *)
  board : Board.b;
  (* scrabble board *)
  bag : Bag.t;
  (* bag of unused tiles *)
  players : Player.t list;
  (* list of players *)
  turn : Player.t;
  (* player whose turn it is currently *)
  game_over : bool; (* whether the game is over *)
}

let init () =
  let players =
    [
      Player.init "Player 1";
      Player.init "Player 2";
      Player.init "Player 3";
      Player.init "Player 4";
    ]
  in
  {
    mode = Draw;
    board = Board.init ();
    players;
    bag = Bag.init ();
    turn = List.hd players;
    game_over = false;
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
  set_color white;
  fill_rect 30 30 140 565;
  set_color black;
  draw_rect 30 30 140 565;
  moveto 50 575;
  draw_string "Key";
  color_key 0xFF0000 " -3x Word" 50 540 25 25;
  color_key 0x0000FF " -3x Letter" 50 510 25 25;
  color_key 0xE88282 " -2x Word" 50 480 25 25;
  color_key 0x8282E8 " -2x Letter" 50 450 25 25;
  Yojson.Basic.from_file "data/key.json"
  |> to_assoc |> List.assoc "letters" |> to_assoc
  |> List.map (fun (k, v) -> (k, to_string v))
  |> List.iteri (fun i (l, num) -> letter_key l num 50 (430 - (i * 15)))

(**[click x y state] is an updated [state] depending on the location [x]
   and [y] of where the mouse clicked.*)
let click x y state =
  match state.mode with
  | Draw ->
      if Bag.clicked x y then
        if Player.num_tiles state.turn < 7 && Bag.count state.bag > 0
        then
          let letter = Bag.find_letter state.bag in
          {
            state with
            turn = Player.add_tile state.turn letter;
            bag = Bag.remove state.bag letter;
          }
        else state
      else if Player.clicked state.turn x y then
        { state with mode = Point x }
      else state
  | Click ->
      if Player.clicked state.turn x y then
        { state with mode = Point x }
      else state
  | Point x0 ->
      if Board.clicked x y state.board then
        {
          state with
          mode = Click;
          turn = Player.remove_tile state.turn x0;
          board =
            Board.add_tile x y (Player.letter state.turn x0) state.board;
        }
      else { state with mode = Click }
  | Init_challenge
  | End_challenge ->
      state
  | Challenge -> state

let game_over state = state.game_over

let draw (state : t) (inpt_op : input option) : unit =
  match state.mode with
  | Draw
  | Click
  | Point _ -> (
      match inpt_op with
      | None -> ()
      | Some _ ->
          Player.draw state.turn;
          Bag.draw state.bag;
          Board.draw state.board)
  | Init_challenge -> ()
  | End_challenge ->
      draw_key ();
      let rec draw_boxes = function
        | [] -> ()
        | h :: t ->
            Player.draw_box h false;
            draw_boxes t
      in
      draw_boxes state.players;
      Player.draw_box state.turn true;
      Player.draw state.turn
  | Challenge -> ()

let init_draw state =
  draw_key ();
  draw state (Some Enter);
  let rec draw_boxes = function
    | [] -> ()
    | h :: t ->
        Player.draw_box h false;
        draw_boxes t
  in
  draw_boxes state.players;
  Player.draw_box state.turn true

(**[next_turn state] is [state] updated to be the next player's turn.*)
let next_turn state =
  let turn_final =
    state.turn |> Player.clear_mem
    |> Player.add_points (Board.score state.board) 
  in
  {
    state with
    turn = Player.next_turn state.turn state.players;
    players = Player.update_player turn_final state.players;
    board = Board.clear_mem state.board;
  }

(**[undo state] is an updated [state] with the most recent tile
   placement of the current player undone.*)
let undo state =
  {
    state with
    turn = Player.undo state.turn;
    board = Board.undo state.board;
  }

let update inpt_op state =
  match state.mode with
  | Init_challenge -> { state with mode = Challenge }
  | End_challenge -> { state with mode = Draw }
  | Draw
  | Click
  | Point _ -> (
      match inpt_op with
      | None -> state
      | Some inpt -> (
          match inpt with
          | Clicked (x, y) -> click x y state
          | Enter -> { state with mode = Init_challenge }
          | Z -> undo state))
  | Challenge -> next_turn { state with mode = End_challenge }
