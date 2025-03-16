open C4_lib.C4_types
open Test_players.Ex_player0
open Test_players.Ex_player1

(* Functor takes in two players to play game of connect 4 *)
module GAME (A : PLAYER_SIG) (B : PLAYER_SIG) : sig
  
  (* Main engine of the game. returns the chip of the winning player *)
  val run_game : board -> chip

end = struct
  (* method to print board for immersion *)
  let print_board (state : board) : unit =
    print_endline "GAME BOARD:";
  
    (* Get number of rows from the first column *)
    let num_rows =
      match state.(0) with
      | Collumn (_, arr) -> Array.length arr
    in
  
    (* Print each row from top (num_rows - 1) to bottom (0) *)
    for row = num_rows - 1 downto 0 do
      let line =
        Array.fold_left (fun acc (Collumn (_, col)) ->
          let symbol =
            match col.(row) with
            | Free -> "*"
            | A -> "A"
            | B -> "B"
          in
          acc ^ symbol ^ " "
        ) "" state
      in
      print_endline line
    done
  
  
  let run_game (state : board) : chip =
    print_board state;
    (* Temporarily return -1 *)
    A
end

module Test_game = GAME (Ex_player0)(Ex_player1)

(* Initialize with an empty playing field *)
let (state : board) = Array.init 7 (fun _ -> Collumn (0, Array.make 6 Free))

let () =
  let _ = Test_game.run_game state in
  ()