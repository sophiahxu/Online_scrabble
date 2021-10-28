open Board
open Players
open Bag

type t = {
  board : Board.b;
  players : Players.player list;
  bag : Bag.b;
}

let draw_tile (state : t) (p : player) : unit =
  raise (Failure "unimplemented")

let init () =
  {
    board = board_setup 0 0;
    players = [ init_player "1" ];
    bag = Bag.init ();
  }