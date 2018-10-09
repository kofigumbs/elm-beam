[![asciicast](https://asciinema.org/a/9XYQWQNlAvqMzTL54JLZSySXA.png)](https://asciinema.org/a/9XYQWQNlAvqMzTL54JLZSySXA)

`elm-beam` compiles Elm to assembly that runs on the Erlang Virtual Machine.
I wrote an essay touching on the interesting parts of this project,
which you can [find on my blog](https://kofi.sexy/blog/elm-beam).
Here are some clarifications I offer in that post:

 - `elm-beam` _is_ a personal exploration
 - `elm-beam` _is_ incomplete (in every sense)
 - `elm-beam` _is NOT_ endoresed by the Elm core team
 - `elm-beam` _is NOT_ "Elm on the server"


## Setup guide

Use [stack](https://docs.haskellstack.org/en/stable/README/)
to build the `elm-beam` executable.

```sh
# make sure you have the `elm-compiler` submodule
git submodule init
git submodule update

# build the executable
stack build

# compile the test .elm file to produce elm.beam
stack exec elm-beam debug/Main.elm

# load the BEAM module into the Erlang console and start the gen_server
erl
1> l(elm).
2> gen_server:start({local, elm}, elm, [], []).
3> gen_server:call(elm, {}).
```


## Contributing

To those interested in continuing the exploration: join us in the _#elm-beam_ channel on the [Elm Slack](https://elmlang.herokuapp.com/).
