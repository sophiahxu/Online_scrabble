type bonus_type = NoBonus

type t = 
{name :string;
location : int * int;
bonus : bonus_type;
letter : char option;}

type b = 
{length : int;
height : int;
tiles : t list;
}

let rec tile_row_setup s num x y row col=
 if num <= 0 then []
 else 
  {name = Char.escaped (Char.chr row) ^ string_of_int col;
  location = (x, y);
  bonus = NoBonus;
  letter = None; } 
  :: tile_row_setup s (num - 1) (x + s) y row (col + 1)

let rec tile_setup s num x y row col= 
  if num <= 0 then []
  else 
    (tile_row_setup s 15 x y row (col)) @ (tile_setup s (num -1) x (y + s) 
    (row + 1) (col + 1))

let board_setup l h = 
  let start_x = l * 1 / 2 in 
  let start_y = h * 1 / 5 in 
  {length = l;
  height = h;
  tiles = tile_setup (l * 2 / 50) 15 start_x start_y 1 1}

let side board = 
  board.length * 2 / 50

let tiles board = 
  board.tiles

(*let name tile = 
  tile.name *)

let tile_x tile = 
  match tile.location with 
  | (x, _) -> x 

let tile_y tile = 
  match tile.location with 
  | (_, y) -> y 

let color tile = 
  match tile.bonus with 
  (*| TripleWord -> Some 0xFF0000
  | TripleLetter -> Some 0x0000FF
  | DoubleWord -> Some 0xE88282
  | DoubleLetter -> Some 0x8282E8*)
  | NoBonus -> None 

let letter tile = 
  match tile.letter with 
  | Some x -> x 
  | None ->  ' ' 