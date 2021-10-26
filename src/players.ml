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

(*[init_player name] creates a player with [name], a starting of 0 points and 
0 tiles*)
let init_player name = 
{
name = name;
point_total = 0;
player_tiles = [];
}

(*[add_points player points] adds [points] to [player]'s current point total*)
let add_points player points = let original = player.point_total in 
{player with point_total = original + points}
 
(*[player_name player] returns the name of [player]*)
let player_name player = player.name

(*[player_points player] returns the current points of [player]*)
let player_points player = player.point_total

(*[player_tiles player] returns a list of the [players] tiles*)
let player_tiles player = player.player_tiles

(*[make tile letter location side] creates a tile with [letter], [location],
and [side]*)
let make_tile letter location side = 
  {
    letter = letter;
    location = location;
    side = side
  }

(*[add_tile player tile] adds tile to [player]'s current tile list. If the 
current tile list is greater than 7, [player]'s tile list will remain the same*)
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

(*[draw_tiles player] draws [player]'s tiles at each tile's respective location*)
let draw_tiles player = let current_tiles = player_tiles player in 
match current_tiles with
| [] -> draw_string ""
| [h] -> let x = x_location h in let y = y_location h in 
moveto x y; draw_char h.letter
| h :: t -> let x = x_location h in let y = y_location h in 
moveto x y; draw_char h.letter


