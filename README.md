### Runing compiled `Test.elm`

```
l(pine).
gen_server:start({local, pine}, pine, [], []).
gen_server:call(pine, {}).
```

### Debugging generated code

```
io:format("~p", [beam_disasm:file(pine)]), x.
```



### TODOs (very high level)

 - currying / partial application / function references
 - don't allocate so much stack space
 - mutual recursion (call_last)

 - Term.Encode/Decode module
 - Native <> NIF
 - Server.Cmd
 - Server.Sub?
