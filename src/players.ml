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

(*[player_name player] returns the name of [player]*)
let player_name player = player.name

(*[player_points player] returns the current points of [player]*)
let player_points player = player.point_total

(*[player_tiles player] returns a list of the [players] tiles*)
let player_tiles player = player.player_tiles

(*[change_location player tile x y] changes the location of [tile] to [(x,y)] 
inside the tile list of [player]*)
let change_location player tile x y = let tile_list = player_tiles player in 
let correct_tile = List.nth tile_list tile in 
correct_tile.location = (x,y)


