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
  | Init_challenge_query
  | Challenge_query
  | Init_challenge
  | Challenge
  | Init_next_turn

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
  challenge : Challenge.t option;
  (* scrabble challenge *)
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
    challenge = None;
  }

let init_end () =
  let players =
    [
      Player.init "Player 1" |> Player.add_points 160;
      Player.init "Player 2" |> Player.add_points 162;
      Player.init "Player 3" |> Player.add_points 171;
      Player.init "Player 4" |> Player.add_points 155;
    ]
  in
  let rec remove num bag =
    if num = 0 then bag
    else
      let letter = Bag.find_letter bag in
      remove (num - 1) (Bag.remove bag letter)
  in

  {
    mode = Draw;
    board = Board.init ();
    players;
    bag = Bag.init () |> remove 85;
    turn = List.hd players;
    game_over = false;
    challenge = None;
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

(**[next_turn state] is [state] updated to be the next player's turn.*)
let rec next_turn state =
  let turn_final =
    state.turn |> Player.clear_mem
    |> Player.add_points (Board.score state.board)
  in
  let st2 =
    {
      state with
      turn = Player.next_turn state.turn state.players;
      players = Player.update_player turn_final state.players;
      board = Board.clear_mem state.board;
    }
  in
  if Player.get_skip st2.turn then
    next_turn { st2 with turn = Player.change_skip st2.turn }
  else st2

(**[challenge_finished state] is the updated state according to the
   outcome of the completed challenge. If the current player lost, then
   we undo everything done during the current turn and skip the players
   next turn as well. If the challenger lost, then we skip that player's
   next turn.*)
let challenge_finished state =
  (*TODO: update so that we either clear the current turn and skip the
    current player's next turn or we just skip loser's next turn*)
  let c =
    match state.challenge with
    | Some c -> c
    | None -> failwith "this shouldn't happen"
  in
  let loser_num =
    match Challenge.loser c with
    | None -> failwith "This shouldn't happen"
    | Some l -> l
  in
  let loser_name = "Player " ^ string_of_int loser_num in
  if
    Player.player_name state.turn = loser_name
    (*turn lost the challenge*)
  then
    next_turn
      {
        state with
        board = Board.undo_all state.board;
        turn = state.turn |> Player.change_skip |> Player.undo_all;
        challenge = None;
        mode = Init_next_turn;
      }
  else
    (*challenger lost the challenge*)
    let challenger_player =
      Player.get_player loser_name state.players
    in
    next_turn
      {
        state with
        players =
          Player.update_player
            (Player.change_skip challenger_player)
            state.players;
        challenge = None;
        mode = Init_next_turn;
      }

(**[end_game state] is state with all of the player's scores updated
   according to official scrabble rules, and the game_over field is set
   to true.*)
let end_game state = { state with game_over = true }

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
  | Challenge_query ->
      if x > 550 && x < 630 && y > 40 && y < 70 then
        {
          state with
          mode = Init_challenge;
          challenge =
            Some
              (Challenge.init
                 ((state.turn |> Player.player_name).[7] |> int_of_char));
        }
      else if x > 650 && x < 730 && y > 40 && y < 70 then
        next_turn { state with mode = Init_next_turn }
      else if
        Bag.count state.bag = 0
        && x > 750 && x < 810
        && float_of_int y
           < (((30. ** 2.) -. ((float_of_int x -. 780.) ** 2.)) ** 0.5)
             +. 55.
        && float_of_int y
           > -.(((30. ** 2.) -. ((float_of_int x -. 780.) ** 2.)) ** 0.5)
             +. 55.
      then end_game state
      else state
  | Init_challenge_query
  | Init_challenge
  | Init_next_turn ->
      state
  | Challenge ->
      let c1 =
        match state.challenge with
        | Some c -> c
        | None -> failwith "this shouldn't happen"
      in
      let return_state =
        { state with challenge = Some (Challenge.click x y c1) }
      in
      let c2 =
        match state.challenge with
        | Some c -> c
        | None -> failwith "this shouldn't happen"
      in
      if Challenge.finished c2 then challenge_finished return_state
      else return_state

let game_over state = state.game_over

(**[challenge_query_draw state] draws the challenge button and the
   continue button. It also draws the done button if the bag is empty.*)
let challenge_query_draw state =
  set_color white;
  fill_rect 450 25 380 60;
  set_color black;
  draw_rect 550 40 80 30;
  draw_rect 650 40 80 30;
  moveto 564 50;
  draw_string "Challenge";
  moveto 667 50;
  draw_string "Continue";
  (*done button*)
  if Bag.count state.bag = 0 then (
    set_color 0x8282E8;
    fill_circle 780 55 30;
    set_color black;
    draw_circle 780 55 30;
    moveto 757 50;
    draw_string "End Game")
  else ()

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
  | Init_challenge_query -> challenge_query_draw state
  | Challenge_query -> ()
  | Init_challenge ->
      set_color white;
      fill_rect 25 25 150 575;
      fill_rect 450 25 380 60;
      Challenge.draw
        (match state.challenge with
        | None -> failwith "this shouldn't happen"
        | Some c -> c)
  | Init_next_turn ->
      draw_key ();
      Board.draw state.board;
      let rec draw_boxes = function
        | [] -> ()
        | h :: t ->
            Player.draw_box h false;
            draw_boxes t
      in
      draw_boxes state.players;
      Player.draw_box state.turn true;
      Player.draw state.turn
  | Challenge -> (
      match inpt_op with
      | Some (Clicked _) ->
          Challenge.draw
            (match state.challenge with
            | None -> failwith "this shouldn't happen"
            | Some c -> c)
      | _ -> ())

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
  | Init_challenge_query -> { state with mode = Challenge_query }
  | Challenge_query -> (
      match inpt_op with
      | None -> state
      | Some inpt -> (
          match inpt with
          | Clicked (x, y) -> click x y state
          | _ -> state))
  | Init_challenge -> { state with mode = Challenge }
  | Init_next_turn -> { state with mode = Draw }
  | Draw
  | Click
  | Point _ -> (
      match inpt_op with
      | None -> state
      | Some inpt -> (
          match inpt with
          | Clicked (x, y) -> click x y state
          | Enter -> { state with mode = Init_challenge_query }
          | Z -> undo state))
  | Challenge -> (
      match inpt_op with
      | None -> state
      | Some inpt -> (
          match inpt with
          | Clicked (x, y) -> click x y state
          | _ -> state))
