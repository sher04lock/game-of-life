# Game Of Life

Game Of Life implementation in Erlang, where every living cell is represented by another process.
Dead cells are just... dead. There is no need to store them as another processes.

## Running

Compile:

    $ rebar3 shell
(compile & run eshell)

or

    $ cd src/ 
    $ erl -make


Running app (Eshell):

    1> application:start(gol).

Raising cells:

    universe:raiseTheDead([{0, 0}, {1, 0}, {2, 0}]).
    
    % or with one of predefined schemas:
    universe:blinker().
    universe:oneForThree().

Iterating:

    3> universe:tick().

Before running simulation it's recommended to open Erlang's `observer` using `observer:start().`

After generating three cells with coords `{0, 0}, {0, 2}, {2, 0}`:
![](https://i.imgur.com/DTUgkzL.png)

After calling universe:tick() to generete next states:

![](https://i.imgur.com/jpcufuJ.png)

All three cells were killed (and also corresponding processes) and one in the middle `{1,1}` of them was born.

