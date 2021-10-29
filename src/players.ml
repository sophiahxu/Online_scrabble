open Graphics

type player_tile = {
  letter : char;
  location : int * int;
  side : int;
}

type player = {
  name : string;
  point_total : int;
  player_tiles : player_tile list;
}

let add_points player points =
  let original = player.point_total in
  { player with point_total = original + points }

let player_name player = player.name

let player_points player = player.point_total

let player_tiles player = player.player_tiles

(**[num_tiles_helper tiles] is the number of nonempty tiles in the
   [tiles].*)
let rec num_tiles_helper (acc : int) (tiles : player_tile list) : int =
  match tiles with
  | [] -> 0
  | h :: t ->
      if h.letter = ' ' then num_tiles_helper acc t
      else num_tiles_helper (acc + 1) t

let num_tiles (p : player) = num_tiles_helper 0 p.player_tiles

let make_tile letter location side = { letter; location; side }

(*[x_location tile] returns the x coordinate of [tile]*)
let x_location tile =
  match tile.location with
  | x, _ -> x

(*[y_location tile] returns the y coordinate of [tile]*)
let y_location tile =
  match tile.location with
  | _, y -> y

let rec draw_tiles player =
  let current_tiles = player_tiles player in
  match current_tiles with
  | [] -> draw_string ""
  | h :: t ->
      let x = x_location h in
      let y = y_location h in
      moveto x y;
      draw_char h.letter;
      draw_tiles { player with player_tiles = t }

let empty_tile1 = make_tile ' ' (456, 30) 10

let empty_tile2 = make_tile ' ' (509, 30) 10

let empty_tile3 = make_tile ' ' (562, 30) 10

let empty_tile4 = make_tile ' ' (615, 30) 10

let empty_tile5 = make_tile ' ' (668, 30) 10

let empty_tile6 = make_tile ' ' (721, 30) 10

let empty_tile7 = make_tile ' ' (774, 30) 10

(*[find_location player] returns all x locations in tile list that are
  an empty tile*)
let rec find_empty player =
  let current_tiles = player_tiles player in
  match current_tiles with
  | [] -> []
  | h :: t ->
      let x = x_location h in
      if h.letter = ' ' then
        x :: find_empty { player with player_tiles = t }
      else find_empty { player with player_tiles = t }

let init_player name =
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
  }

(*[add_tile_list player_tiles letter] replaces an empty tile in
  [player_tiles] with a new tile with [letter]*)
let rec change_tile player_tiles letter =
  match player_tiles with
  | [] -> []
  | h :: t ->
      if h.letter = ' ' then { h with letter } :: t
      else h :: change_tile t letter

let add_tile player letter =
  let current_tiles = player_tiles player in
  { player with player_tiles = change_tile current_tiles letter }
