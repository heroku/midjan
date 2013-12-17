-module(midjan_translator).

-type translator() :: module().
-export_type([translator/0]).

-callback run(Module, State) ->
    {next, State}|
    {{back, midjan_module:work()}, State}|
    {stop, State} when
      Module :: midjan_module:work(),
      State :: midjan_module:state().
