open C4_lib.C4_types

module Ex_player0 : PLAYER_SIG = struct
  (* Players that always plays collumn 0 *)
  let make_move (_state : board) : int =
    0
end