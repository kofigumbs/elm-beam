```
1> l(pine).
{module,pine}
2> gen_server:start({local, pine}, pine, [], []).
{ok,<0.63.0>}
3> gen_server:call(pine, {}).
2
4> gen_server:call(pine, {}).
3
```
