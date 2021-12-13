open Graphics

(**represents the input mode that the challenge state is in*)
type mode =
  | Challenger_query
  | Dictionary_lookup
  | Loser_query
  | Continue_query

type t = {
  mode : mode;
  loser : int option;
  finished : bool;
}

(*[draw_rects num] draws a row of rectangles, with its associated number
  inside it, with lower left corner at [y]*)
let rec draw_rects num y =
  let x = 40 + (30 * (num - 1)) in
  match num with
  | 0 -> draw_string ""
  | _ ->
      draw_rect x y 30 30;
      moveto (x + 12) (y + 10);
      draw_string (string_of_int num);
      draw_rects (num - 1) y

let query () =
  set_color white;
  fill_rect 30 30 140 565;
  set_color blue;
  draw_rect 30 30 140 565;
  moveto 50 575;
  draw_string "Challenge Mode!";
  moveto 45 555;
  set_color black;
  draw_string "Which Player is";
  moveto 45 540;
  draw_string "Challenging?";
  draw_rects 4 500

let which_player x =
  if x > 40 && x < 70 then 1
  else if x > 70 && x < 100 then 2
  else if x > 100 && x < 130 then 3
  else if x > 130 && x < 160 then 4
  else 0

let return_int () =
  let s = wait_next_event [ Button_down ] in
  let x = s.mouse_x in
  which_player x

let look_up () =
  (* let s = wait_next_event [ Button_down ] in let x = s.mouse_x in let
     num = which_player x in *)
  moveto 40 475;
  draw_string "Players, look up in ";
  moveto 40 460;
  draw_string "a dictionary whether ";
  moveto 40 445;
  draw_string "the word is valid.";
  moveto 40 430;
  draw_string "Click DONE when ";
  moveto 40 415;
  draw_string "finished.";
  draw_rect 70 375 50 30;
  moveto 85 385;
  draw_string "DONE"

let loser () =
  (* let s = wait_next_event [ Button_down ] in if s.mouse_x > 70 &&
     s.mouse_x < 120 && s.mouse_y > 375 && s.mouse_y < 415 then*)
  moveto 40 350;
  draw_string "Which player lost the";
  moveto 40 335;
  draw_string "challenge?";
  draw_rects 4 295

let continue () =
  (* let s = wait_next_event [ Button_down ] in let x = s.mouse_x in let
     num = which_player x in *)
  moveto 40 270;
  draw_string "The losing player's";
  moveto 40 255;
  draw_string "turn will be skipped.";
  moveto 40 240;
  draw_string "Click return to go";
  moveto 40 225;
  draw_string "back to the game.";
  draw_rect 70 180 50 30;
  moveto 80 190;
  draw_string "Return"

let init () =
  { mode = Challenger_query; loser = None; finished = false }

let draw c =
  match c.mode with
  | Challenger_query -> query ()
  | Dictionary_lookup -> look_up ()
  | Loser_query -> loser ()
  | Continue_query -> continue ()

let click x y c =
  match c.mode with
  | Challenger_query ->
      if y > 500 && y < 530 && x > 40 && x < 160 then
        { mode = Dictionary_lookup; loser = None; finished = false }
      else c
  | Dictionary_lookup ->
      if x > 70 && x < 120 && y > 375 && y < 415 then
        { mode = Loser_query; loser = None; finished = false }
      else c
  | Loser_query ->
      if y > 295 && y < 325 then
        let num = Some (which_player x) in
        { mode = Continue_query; loser = num; finished = false }
      else c
  | Continue_query -> { c with finished = true }

let loser c = c.loser
let finished c = c.finished
