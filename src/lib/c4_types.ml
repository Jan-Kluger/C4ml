type chip =
  | A
  | B
  | Free

(* for more efficient insertion into collumns *)
(* Int is a counter for how full the collumn is *)
type collumn = Collumn of (int * chip array)

(* collection fo the collumns *)
type board = collumn array

module type PLAYER_SIG = sig
  (* Player takes in playing field and returns collumn to drop chip *)
  val make_move : board -> int
end