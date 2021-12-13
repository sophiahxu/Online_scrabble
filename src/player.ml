open Graphics

type player_tile = {
  letter : string;
  location : int * int;
  side : int;
}

type t = {
  name : string;
  point_total : int;
  player_tiles : player_tile list;
  skip : bool;
  memory_stack : string list;
}

let add_points points (p : t) =
  let original = p.point_total in
  { p with point_total = original + points }

let player_name (p : t) = p.name

let player_points (p : t) = p.point_total

(**[num_tiles_helper tiles] is the number of nonempty tiles in the
   [tiles].*)
let rec num_tiles_helper (acc : int) (tiles : player_tile list) : int =
  match tiles with
  | [] -> acc
  | h :: t ->
      if h.letter = "" then num_tiles_helper acc t
      else num_tiles_helper (acc + 1) t

let num_tiles (p : t) = num_tiles_helper 0 p.player_tiles

(*[make_tile letter location side] makes a tile with [letter],
  [location], and [side]*)
let make_tile letter location side = { letter; location; side }

(*[x_location tile] returns the x coordinate of [tile]*)
let x_location tile =
  match tile.location with
  | x, _ -> x

(*[y_location tile] returns the y coordinate of [tile]*)
let y_location tile =
  match tile.location with
  | _, y -> y

(**[draw_blank tile] draws a white rectangle at [tile]'s location*)
let draw_blank tile =
  let x = x_location tile in
  let y = y_location tile in
  set_color white;
  fill_rect (x + 15) (y + 15) 24 24

(**[draw_letter tile] draws a beige rectangle at [tile]'s location*)
let draw_letter tile =
  let x = x_location tile in
  let y = y_location tile in
  set_color 0xfce283;
  fill_rect (x + 15) (y + 15) 24 24;
  set_color black;
  moveto (x + 23) (y + 23);
  draw_string tile.letter

(** [draw_helper tiles] draws the tiles in [tiles] to the screen. *)
let rec draw_helper tiles =
  match tiles with
  | [] -> draw_string ""
  | h :: t ->
      if h.letter = "" then draw_blank h else draw_letter h;
      draw_helper t

(**[draw_row w h num x y] draws a row of [num] rectangles of width [w]
   and height [h]. The lower left corner of the row begins at the
   coordinates ([x], [y]) Requires: [w], [h] > 0. [num], [x], [y] >= 0.*)
let rec draw_row w h num x y =
  if num <= 0 then () else draw_rect x y w h;
  if num > 0 then draw_row w h (num - 1) (x + w) y

let draw p =
  set_color white;
  fill_rect 450 25 380 60;
  set_color black;
  draw_row 53 53 7 456 30;
  draw_helper p.player_tiles

let empty_tile1 = make_tile "" (456, 30) 10

let empty_tile2 = make_tile "" (509, 30) 10

let empty_tile3 = make_tile "" (562, 30) 10

let empty_tile4 = make_tile "" (615, 30) 10

let empty_tile5 = make_tile "" (668, 30) 10

let empty_tile6 = make_tile "" (721, 30) 10

let empty_tile7 = make_tile "" (774, 30) 10

let init name =
  {
    name;
    point_total = 0;
    player_tiles =
      [
        empty_tile1;
        empty_tile2;
        empty_tile3;
        empty_tile4;
        empty_tile5;
        empty_tile6;
        empty_tile7;
      ];
    skip = false;
    memory_stack = [];
  }

(**[add_tile_list player_tiles letter] replaces an empty tile in
   [player_tiles] with a new tile with [letter]*)
let rec change_tile player_tiles letter =
  match player_tiles with
  | [] -> []
  | h :: t ->
      if h.letter = "" then { h with letter } :: t
      else h :: change_tile t letter

let add_tile (p : t) letter =
  let current_tiles = p.player_tiles in
  { p with player_tiles = change_tile current_tiles letter }

(**[lower_left st] returns the lower left corner of the rectangle
   surrounding the current point [st]*)
let lower_left x =
  if x > 456 && x < 509 then 456
  else if x > 509 && x < 562 then 509
  else if x > 562 && x < 615 then 562
  else if x > 615 && x < 668 then 615
  else if x > 668 && x < 721 then 668
  else if x > 721 && x < 774 then 721
  else if x > 774 && x < 827 then 774
  else 0

(**[remove_tile_helper player_tiles location] removes the the tile at
   [location] within player_tiles*)
let rec remove_tile_helper player_tiles location =
  match player_tiles with
  | [] -> []
  | h :: t ->
      let x = x_location h in
      if x = lower_left location then { h with letter = "" } :: t
      else h :: remove_tile_helper t location

(**[return letter player_tiles location] returns the tile letter at
   specified [location] inside [player_tiles]*)
let rec return_letter player_tiles location =
  match player_tiles with
  | [] -> ""
  | h :: t ->
      let x = x_location h in
      if x = lower_left location then h.letter
      else return_letter t location

let remove_tile (p : t) x =
  let current_tiles = p.player_tiles in
  let p_letter = return_letter current_tiles x in
  {
    p with
    player_tiles = remove_tile_helper current_tiles x;
    memory_stack = p_letter :: p.memory_stack;
  }

(**[clicked_helper p x y] returns the tile at location [(x,y)] inside
   [p]*)
let rec clicked_helper (p : t) x y =
  if y > 30 && y < 83 then
    let location = lower_left x in
    let current_tiles = p.player_tiles in
    match current_tiles with
    | [] -> empty_tile1
    | h :: t ->
        if x_location h = location then h
        else clicked_helper { p with player_tiles = t } x y
  else empty_tile1

let clicked (p : t) x y =
  let tile = clicked_helper p x y in
  if tile.letter = "" then false else true

let letter (p : t) l =
  let tile = clicked_helper p l 35 in
  tile.letter

let rec update_player p p_list =
  match p_list with
  | [] -> failwith "Player not found"
  | h :: t -> if h.name = p.name then p :: t else h :: update_player p t

let next_turn p p_list =
  let rec next_turn_aux lst =
    match lst with
    | [] -> failwith "Player not found"
    | [ h ] -> raise (Failure "Last element")
    | h1 :: h2 :: t ->
        if h1.name = p.name then h2 else next_turn_aux (h2 :: t)
  in
  try next_turn_aux p_list with
  | Failure _ -> List.hd p_list

let get_skip p = p.skip

let change_skip p =
  match p.skip with
  | true -> { p with skip = false }
  | false -> { p with skip = true }

let undo p =
  match p.memory_stack with
  | [] -> p
  | h :: t ->
      let new_p = { p with memory_stack = t } in
      add_tile new_p h

let rec undo_all p =
  match p.memory_stack with
  | [] -> p
  | h :: t -> undo_all (undo p)

let draw_box p status =
  let num =
    match p.name with
    | "Player 1" -> 1
    | "Player 2" -> 2
    | "Player 3" -> 3
    | _ -> 4
  in
  if status = true then set_color 0xD3F2F6 else set_color white;
  let x = 208 in
  let y = (100 * (4 - num)) + 180 in
  fill_rect x y 160 90;
  if status = true then set_color blue else set_color black;
  draw_rect x y 160 90;
  if status = true then set_color blue else set_color black;
  moveto (x + 16) (y + 72);
  draw_string ("Player " ^ string_of_int num);
  moveto (x + 25) (y + 40);
  draw_string (string_of_int p.point_total)

let clear_mem p = { p with memory_stack = [] }

let rec get_player name = function
  | [] -> failwith "invalid name argument"
  | h :: t -> if name = h.name then h else get_player name t
