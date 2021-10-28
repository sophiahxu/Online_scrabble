open Board
open Players
open Bag

type info = {
  board : Board.b;
  players : Players.player list;
  bag : Bag.b;
}

type t =
  | Loading_initial
  | Initial
  | Loading_active
  | Active of info
  | Loading_complete
  | Complete

let draw_tile (i : info) (p : player) : unit =
  raise (Failure "unimplemented")

let init_info () =
  {
    board = board_setup 0 0;
    players = [ init_player "1" ];
    bag = Bag.init_bag ();
  }