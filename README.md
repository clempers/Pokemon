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

Some other flags to know about:

-b       : enable pokemon to learn and use buff moves

-w x     : sets the width of the pokemon grid to x.   So there will be x * x pokemon

-d x     : sets the display width of each pokemon to x.  -d 1 makes each pokemon a pixel

-disable : blacks out all pokemon not being used in calculation.  Looks really cool

-s x     : Saves output to x if not loading.  Loads from x if is loading

-load    : Loads previous run instead of running new one.  Requires -s to be used as well.
