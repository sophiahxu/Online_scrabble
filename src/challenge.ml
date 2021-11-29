open Graphics

(*just for testing purposes, delete later*)
let testing = open_graph ""; resize_window 900 625

(*[draw_rects num] draws a row of rectangles, with its associated number inside 
  it, with lower left corner at [y]*)
let rec draw_rects num y = 
  let x = 40 + 30*(num - 1) in
  match num with
  | 0 -> draw_string ""
  | _ -> draw_rect x y 30 30; 
    moveto (x + 12) (y + 10);
    draw_string (string_of_int num);
    draw_rects (num - 1) y

let start = 
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

let look_up = 
  let s = wait_next_event [Button_down] in
  let x = s.mouse_x in
  let num = which_player x in 
  moveto 40 475;
  draw_string ("Player" ^ (string_of_int num) ^ ", look up in ");
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

let question = 
  let s = wait_next_event [Button_down] in
  if s.mouse_x > 70 && s.mouse_x < 120 && 
    s.mouse_y > 375 && s.mouse_y < 415 then
    moveto 40 350;
    draw_string "Which player lost the";
    moveto 40 335;
    draw_string "challenge?";
    draw_rects 4 295

let winner = 
  let s = wait_next_event [Button_down] in
  let x = s.mouse_x in
  let num = which_player x in 
  moveto 40 270;
  draw_string ("Player" ^ (string_of_int num) ^ ", your next");
  moveto 40 255;
  draw_string "turn will be skipped.";
  num

let init = 
  start;
  look_up;
  question;
  winner