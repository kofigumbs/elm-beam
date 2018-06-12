### Runing compiled `Test.elm`

```
l(pine).  %% => {module,pine}
gen_server:start({local, pine}, pine, [], []).
%% => {ok,<0.63.0>}
gen_server:call(pine, {}).
%% => 2
gen_server:call(pine, {}).
%% => 3
```

### Debugging generated code

```
io:format("~p", [beam_disasm:file(pine)]), ''.
```



# TODOs (very high level)

### Semantics

 - currying / partial application / function references
 - don't allocate so much stack space
 - mutual recursion (call_last)

### Architecture

 - Term.Encode/Decode module
 - Native <> NIF
 - Server.Cmd
 - Server.Sub?
