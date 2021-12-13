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
  challenger : int option;
  victim : int;
}

let init num =
  {
    mode = Challenger_query;
    loser = None;
    finished = false;
    challenger = None;
    victim = num;
  }

(*[create_list lst c] creates a list without the integer number of [c]'s
  victim*)
let create_list lst c =
  let victim = c.victim in
  List.filter (fun a -> a <> victim) lst

(*[draw_rects num] draws a row of rectangles, with its associated number
  inside it, with lower left corner at [y]*)
let rec draw_rects num lst y =
  let x = 55 + (30 * (num - 1)) in
  match num with
  | 0 -> draw_string ""
  | _ ->
      draw_rect x y 30 30;
      moveto (x + 12) (y + 10);
      draw_string (string_of_int (List.hd lst));
      draw_rects (num - 1) (List.tl lst) y

let query c =
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
  draw_rects 3 (create_list [ 4; 3; 2; 1 ] c) 500

let look_up c =
  let challenger =
    match c.challenger with
    | None -> failwith "should not happen"
    | Some i -> i
  in
  moveto 40 475;
  draw_string ("Player " ^ string_of_int challenger ^ ", look up in ");
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

let draw_nums c =
  let challenger =
    match c.challenger with
    | None -> failwith "can't happen"
    | Some i -> i
  in
  let victim = c.victim in
  moveto 77 305;
  draw_string (string_of_int challenger);
  moveto 107 305;
  draw_string (string_of_int victim)

let rec draw_two num y =
  let x = 65 + (30 * (num - 1)) in
  match num with
  | 0 -> draw_string ""
  | _ ->
      draw_rect x y 30 30;
      draw_two (num - 1) y

let loser c =
  moveto 40 350;
  draw_string "Which player lost the";
  moveto 40 335;
  draw_string "challenge?";
  draw_two 2 295;
  draw_nums c

let continue c =
  let loser =
    match c.loser with
    | None -> failwith "should not happen"
    | Some i -> i
  in
  moveto 40 270;
  draw_string ("Player " ^ string_of_int loser ^ ", your next ");
  moveto 40 255;
  draw_string "turn will be skipped.";
  moveto 40 240;
  draw_string "Click return to go";
  moveto 40 225;
  draw_string "back to the game.";
  draw_rect 70 180 50 30;
  moveto 80 190;
  draw_string "Return"

let draw c =
  match c.mode with
  | Challenger_query -> query c
  | Dictionary_lookup -> look_up c
  | Loser_query -> loser c
  | Continue_query -> continue c

let challenger_query x y c =
  let lst = create_list [ 1; 2; 3; 4 ] c in
  if y > 500 && y < 530 then
    if x > 55 && x < 85 then
      {
        c with
        mode = Dictionary_lookup;
        challenger = Some (List.hd lst);
      }
    else if x > 85 && x < 115 then
      {
        c with
        mode = Dictionary_lookup;
        challenger = Some (List.nth lst 1);
      }
    else if x > 115 && x < 145 then
      {
        c with
        mode = Dictionary_lookup;
        challenger = Some (List.nth lst 2);
      }
    else c
  else c

let click x y c =
  match c.mode with
  | Challenger_query -> challenger_query x y c
  | Dictionary_lookup ->
      if x > 70 && x < 120 && y > 375 && y < 415 then
        { c with mode = Loser_query }
      else c
  | Loser_query ->
      if y > 295 && y < 325 then
        if x > 65 && x < 95 then
          { c with mode = Continue_query; loser = c.challenger }
        else if x > 95 && x < 125 then
          { c with mode = Continue_query; loser = Some c.victim }
        else c
      else c
  | Continue_query -> { c with finished = true }

let loser c = c.loser
let finished c = c.finished
