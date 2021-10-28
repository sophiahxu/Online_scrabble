open Graphics 

type player_tile = 
{
letter: char;
location: int * int;
side: int
}

type player = 
{
name: string;
point_total: int;
player_tiles: player_tile list
}

let add_points player points = let original = player.point_total in 
{player with point_total = original + points}
 
let player_name player = player.name

let player_points player = player.point_total

let player_tiles player = player.player_tiles

let make_tile letter location side = 
  {
    letter = letter;
    location = location;
    side = side
  }

let add_tile player tile = let current_tiles = player_tiles player in 
if List.length current_tiles < 7 then 
  match current_tiles with 
| [] -> {player with player_tiles = [tile]} 
| h :: t -> {player with player_tiles = tile :: current_tiles}
else {player with player_tiles = current_tiles}

(*[x_location tile] returns the x coordinate of [tile]*)
let x_location tile = match tile.location with 
| (x, _) -> x

(*[y_location tile] returns the y coordinate of [tile]*)
let y_location tile = match tile.location with 
| (_, y) -> y

let rec draw_tiles player = let current_tiles = player_tiles player in 
match current_tiles with
| [] -> draw_string ""
| h :: t -> let x = x_location h in let y = y_location h in 
moveto x y; draw_char h.letter; draw_tiles {player with player_tiles = t}

let empty_tile1 = make_tile ' ' (456, 30) 0

let empty_tile2 = make_tile ' ' (509, 30) 0

let empty_tile3 = make_tile ' ' (562, 30) 0

let empty_tile4 = make_tile ' ' (615, 30) 0

let empty_tile5 = make_tile ' ' (668, 30) 0

let empty_tile6 = make_tile ' ' (721, 30) 0

let empty_tile7 = make_tile ' ' (774, 30) 0

(*[find_location player] returns all x locations in tile list that are an 
empty tile*)
let rec find_empty player = let current_tiles = 
  player_tiles player in 
match current_tiles with 
| [] -> []
| h :: t -> let x = x_location h in if h.side = 0 then 
   x :: find_empty {player with player_tiles = t} else 
    find_empty {player with player_tiles = t}

let init_player name = 
  {
  name = name;
  point_total = 0;
  player_tiles = [empty_tile1; empty_tile2; empty_tile3; empty_tile4; 
  empty_tile5; empty_tile6; empty_tile7  ];
  }
