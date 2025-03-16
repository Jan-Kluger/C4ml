open C4_lib.Player_sig

(* Functor takes in two players to play game of connect 4 *)
module GAME (A : PLAYER_SIG) (B : PLAYER_SIG) = struct
  let make_move (state : chip array array) : int =
    (* Initialize with an empty playing field *)
    let state = Array.init 7 (fun _ -> Array.make 7 Free) in


    (* Temporarily return -1 *)
    (-1)
end

let () = print_endline "Hello, World!"
