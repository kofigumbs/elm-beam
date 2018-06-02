### Runing compiled `Test.elm`

```
l(pine).
%% => {module,pine}
gen_server:start({local, pine}, pine, [], []).
%% => {ok,<0.63.0>}
gen_server:call(pine, {}).
%% => 2
gen_server:call(pine, {}).
%% => 3
```

### Debugging generated code

```
beam_disasm:file(pine).
```
