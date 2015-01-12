%%% @doc Behaviour / Callback for a translator module
%%% The translator module can be used as a mechanism to transform the output
%%% of some existing modules into something midjan understands and can execute.
%%% Can be seen as a wrapper around any worker.
-module(midjan_translator).

-type translator() :: module().
-export_type([translator/0]).

-callback run(Module, State) ->
    {next, State}|
    {{back, midjan_module:work()}, State}|
    {stop, State} when
      Module :: midjan_module:work(),
      State :: midjan_module:state().
