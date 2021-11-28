open Graphics

(*just for testing purposes, delete later*)
let testing = open_graph ""; resize_window 900 625

(*[draw_rects num] draws a row of rectangles, with its associated number inside 
  it*)
let rec draw_rects num = 
  let x = 40 + 30*(num - 1) in
  let y = 500 in
  match num with
  | 0 -> draw_string ""
  | _ -> draw_rect x y 30 30; 
    moveto (x + 12) (y + 10);
    draw_string (string_of_int num);
    draw_rects (num - 1)

let init = 
  set_color blue;
  draw_rect 30 30 140 565;
  moveto 50 575;
  draw_string "Challenge Mode!";
  moveto 45 555;
  set_color black;
  draw_string "Which Player is";
  moveto 45 540;
  draw_string "Challenging?";
  draw_rects 4

let which_player = 
  let s = wait_next_event [Button_down] in 
  let x = s.mouse_x in
    if x > 40 && x < 70 then 1
    else if x > 70 && x < 100 then 2
    else if x > 100 && x < 130 then 3
    else 4

let look_up status = 
  if status = true then 
  let num = which_player in 
  moveto 40 475;
  draw_string ("Player" ^ (string_of_int num) ^ ", look up in ");
  moveto 40 460;
  draw_string "a dictionary whether ";
  moveto 40 445;
  draw_string "the word is valid"
else draw_string ""