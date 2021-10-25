open Graphics

(**[graph] opens the graph that will contain all of the other components
   of the board and screen.*)
let graph = open_graph " 900x625"

(**[draw_row w h num x y] draws a row of [num] rectangles of width [w]
   and height [h]. The lower left corner of the row begins at the
   coordinates ([x], [y]) Requires: [w], [h] > 0. [num], [x], [y] >= 0.*)
let rec draw_row w h num x y =
  if num <= 0 then () else draw_rect x y w h;
  if num > 0 then draw_row w h (num - 1) (x + w) y;
  moveto x y

(**[draw_grid w h x_num y_num x y] draws a grid of rectangles that are
   each width [w] and height [h]. The overall grid is [x_num] rectangles
   long and [y_num] rectangles tall. The lowerleft corner of the entire
   grid is at ([x], [y]). Requires: [w], [h], [x_num], [y_num] > 0. [x],
   [y] >= 0.*)
let rec draw_grid w h x_num y_num x y =
  if y_num <= 0 then () else draw_row w h x_num x y;
  if y_num > 0 then draw_grid w h x_num (y_num - 1) x (y + h);
  moveto x y

(**[triple_word h l x y] is an array containing information about the
   locations and colors that need to be filled in with the color for the
   triple word score tiles. Each tile is of height [h] and length [l],
   and the lower left corner of the entire board is at ([x], [y]). Each
   entry in the array is of the format ((x : int), (y : int), color :
   int). *)
let triple_word h l x y =
  let color = 0xFF0000 in
  let row0 = y in
  let row7 = y + (7 * h) in
  let row14 = y + (14 * h) in
  let col0 = x in
  let col7 = x + (7 * l) in
  let col14 = x + (14 * l) in

  [
    ((col0, row0), color);
    ((col0, row7), color);
    ((col0, row14), color);
    ((col7, row0), color);
    ((col7, row14), color);
    ((col14, row0), color);
    ((col14, row7), color);
    ((col14, row14), color);
  ]

(**[triple_letter h l x y] is an array containing information about the
   locations and colors that need to be filled in with the color for the
   triple letter score tiles. Each tile is of height [h] and length [l],
   and the lower left corner of the entire board is at ([x], [y]). Each
   entry in the array is of the format ((x : int), (y : int), color :
   int). *)
let triple_letter h l x y =
  let color = 0x0000FF in
  let row1 = y + h in
  let row5 = y + (5 * h) in
  let row9 = y + (9 * h) in
  let row13 = y + (13 * h) in

  let col1 = x + l in
  let col5 = x + (5 * l) in
  let col9 = x + (9 * l) in
  let col13 = x + (13 * l) in

  [
    ((col5, row1), color);
    ((col9, row1), color);
    ((col1, row5), color);
    ((col5, row5), color);
    ((col9, row5), color);
    ((col13, row5), color);
    ((col5, row9), color);
    ((col1, row9), color);
    ((col9, row9), color);
    ((col13, row9), color);
    ((col5, row13), color);
    ((col9, row13), color);
  ]

(**[double_word h l x y] is an array containing information about the
   locations and colors that need to be filled in with the color for the
   double word score tiles. Each tile is of height [h] and length [l],
   and the lower left corner of the entire board is at ([x], [y]). Each
   entry in the array is of the format ((x : int), (y : int), color :
   int). *)
let double_word h l x y =
  let color = 0xE88282 in
  let row1 = y + h in
  let row2 = y + (2 * h) in
  let row3 = y + (3 * h) in
  let row4 = y + (4 * h) in
  let row7 = y + (7 * h) in
  let row10 = y + (10 * h) in
  let row11 = y + (11 * h) in
  let row12 = y + (12 * h) in
  let row13 = y + (13 * h) in

  let col1 = x + l in
  let col2 = x + (2 * l) in
  let col3 = x + (3 * l) in
  let col4 = x + (4 * l) in
  let col7 = x + (7 * l) in
  let col10 = x + (10 * l) in
  let col11 = x + (11 * l) in
  let col12 = x + (12 * l) in
  let col13 = x + (13 * l) in

  [
    ((col1, row1), color);
    ((col13, row1), color);
    ((col2, row2), color);
    ((col12, row2), color);
    ((col3, row3), color);
    ((col11, row3), color);
    ((col4, row4), color);
    ((col10, row4), color);
    ((col7, row7), color);
    ((col4, row10), color);
    ((col10, row10), color);
    ((col3, row11), color);
    ((col11, row11), color);
    ((col2, row12), color);
    ((col12, row12), color);
    ((col1, row13), color);
    ((col13, row13), color);
  ]

(**[double_letter h l x y] is an array containing information about the
   locations and colors that need to be filled in with the color for the
   double letter score tiles. Each tile is of height [h] and length [l],
   and the lower left corner of the entire board is at ([x], [y]). Each
   entry in the array is of the format ((x : int), (y : int), color :
   int). *)
let double_letter h l x y =
  let color = 0x8282E8 in
  let row0 = y in
  let row2 = y + (2 * h) in
  let row3 = y + (3 * h) in
  let row6 = y + (6 * h) in
  let row7 = y + (7 * h) in
  let row8 = y + (8 * h) in
  let row11 = y + (11 * h) in
  let row12 = y + (12 * h) in
  let row14 = y + (14 * h) in

  let col0 = x in
  let col2 = x + (2 * l) in
  let col3 = x + (3 * l) in
  let col6 = x + (6 * l) in
  let col7 = x + (7 * l) in
  let col8 = x + (8 * l) in
  let col11 = x + (11 * l) in
  let col12 = x + (12 * l) in
  let col14 = x + (14 * l) in

  [
    ((col3, row0), color);
    ((col11, row0), color);
    ((col6, row2), color);
    ((col8, row2), color);
    ((col0, row3), color);
    ((col7, row3), color);
    ((col14, row3), color);
    ((col2, row6), color);
    ((col6, row6), color);
    ((col8, row6), color);
    ((col12, row6), color);
    ((col3, row7), color);
    ((col11, row7), color);
    ((col2, row8), color);
    ((col6, row8), color);
    ((col8, row8), color);
    ((col12, row8), color);
    ((col0, row11), color);
    ((col7, row11), color);
    ((col14, row11), color);
    ((col6, row12), color);
    ((col8, row12), color);
    ((col3, row14), color);
    ((col11, row14), color);
  ]

(**[color_list h l x y] is an array of the necessary locations and
   colors used to fill in the grid made up of squares of height [h] and
   length [l], where the lower left corner of the grid is at [x] [y].
   Each entry in the array is of the format ((x : int), (y : int), color
   : int). *)
let color_list h l x y =
  triple_word h l x y @ double_letter h l x y @ double_word h l x y
  @ triple_letter h l x y

(**[color_grid lst l h] fills in the grid at the locations and with the
   colors described in [lst]. Each unit of the grid has length [l] and
   height [h].*)
let rec color_grid lst l h =
  match lst with
  | [] -> ()
  | ((x, y), col) :: t ->
      set_color col;
      fill_rect x y h l;
      color_grid t l h

(*[color_key color phrase x y w h] draws a rectangle with lower left
  corner at ([x],[y]) with width [w] and height [h] and colors it with
  [color]. [phrase] is displayed to the right of the rectangle*)
let color_key color phrase x y w h =
  set_color color;
  draw_rect x y w h;
  fill_rect x y w h;
  moveto (x + w) (y + (h / 2));
  set_color black;
  draw_string phrase

(*[letter_key letter number] prints a string with lower left corner at
  ([x],[y]) that associates [letter] with [number]*)
let letter_key letter number x y =
  moveto x y;
  draw_string (Printf.sprintf "%s - %s" letter number)

(**[make board ()] sets up the initial board game, which includes the
   Scrabble board itself, a space for the current player's letters, a
   button to click to draw letters, and two spaces for player scores to
   be displayed. If a key is pressed, the GUI closes. *)
let make_board () =
  graph;

  let length = 35 in
  let height = 35 in
  let start_x = 350 in
  let start_y = 80 in
  set_color black;
  draw_grid length height 15 15 start_x start_y;

  (*draws the main board*)
  let colors = color_list length height start_x start_y in
  color_grid colors length height;

  set_color black;
  draw_grid 40 40 7 1 490 20;

  (*draws the tiles*)
  draw_rect 30 30 125 565;
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
  letter_key "K" "5" 50 310;
  letter_key "L" "1" 50 295;
  letter_key "M" "3" 50 280;
  letter_key "N" "1" 50 265;
  letter_key "Q" "10" 50 250;
  letter_key "R" "1" 50 235;
  letter_key "U" "1" 50 220;
  letter_key "V" "4" 50 205;
  letter_key "O" "1" 50 190;
  letter_key "P" "3" 50 175;
  letter_key "S" "1" 50 160;
  letter_key "T" "1" 50 145;
  letter_key "W" "4" 50 130;
  letter_key "X" "8" 50 115;
  letter_key "Y" "4" 50 100;
  letter_key "Z" "10" 50 85;

  (*draws key box*)
  draw_rect 180 445 150 100;
  draw_rect 180 310 150 100;

  (*draws player score boxes*)
  moveto 190 525;
  draw_string "Player 1";
  moveto 190 390;
  draw_string "Player 2";

  (*draw player score spaces*)
  draw_circle 255 80 55;
  moveto 230 85;
  draw_string "DRAW";
  moveto 220 70;
  draw_string "98 tiles left"
