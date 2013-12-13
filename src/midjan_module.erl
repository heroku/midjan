-module(midjan_module).

-type work() :: module().
-export_type([work/0]).

-callback run(State) ->
    {next, State}|
    {{back, work()}, State}|
    {stop, State} when
      State :: any().
