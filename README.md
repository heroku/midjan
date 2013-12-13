# Midjan

Midjan provides a way to have a chain of modules to run, go back in
the chain and rerun it from a a certain point.

## API

### Setup

``` erlang
-type opts() :: [opt()]|[].
-type opt() :: {ordered, [midja_module:work()]}|
               {translator, midja_translator:translator()}|
               {aftereach, midja_module:work()}.
-spec start(any(), opts()) ->
                   {done, any()}|
                   no_return().
                 
```

### Midja Work Module

The Work module does the actual work, and returns a new state. It can
also return a `{back, OtherModule}` to go back in the ordered list and
run that module, and other modules after it in the list.

``` erlang
-callback run(State) ->
    {next, State}|
    {{back, module()}, State}|
    {stop, State} when
      State :: any().
```

### Midja Translator

Midja has a strict API, and defining a translator makes it easier to
interact with other modules you might already have. Instead of calling
the module you defined in your work list directly, it calls
`Translator:run(ModuleToRunNext, State)`. It should have the same
return values as a Midja Work Module.

``` erlang
-callback run(Module, State) ->
    {next, State}|
    {{back, midjan_module:work()}, State}|
    {stop, State} when
      Module :: midjan_module:work(),
      State :: any().
```
