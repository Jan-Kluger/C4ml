# C4ml
Connect 4 game in OCaml.

### How to use
Player implementations can be found in ``` src/test_players/ ```. Currently there are only dummy players. However you can add whatever players you like. Just be sure to open them in Main.

After adding players, opening and inserting in functor build with ```dune build```.

Running ``` dune exec _build/default/bin/main.exe ``` Will give the history of the bots and return the winning bot. If there is none it will result in a draw.
