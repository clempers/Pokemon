# Pokemon
Pokemon battle royale

In order to compile it requires the ocaml compiler but no additional packages. To compile run:

ocamlc -o pokemon graphics.cma unix.cma pokemon.ml

the output will then be named pokemon.

When running the program clicking anywhere will bring up an info window of the pokemon clicked on.
Pressing q will exit the program.
Pressing space will pause/unpause the program.
While paused moving the mouse will not redraw the info window and will only dissapear after unclicking

In order to save the output use the -s argument.  As an example

./pokemon -s save.txt

You can quit at anytime and the file will save the output up to that point.
In order to load the past run add the flag -load as an example.

./pokemon -s save.txt -load

This will then read the file save.txt and replay the run.
