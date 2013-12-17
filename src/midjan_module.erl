-module(midjan_module).

-type work() :: module().
-type state() :: any().
-export_type([work/0,
              state/0]).

-callback run(State) ->
    {next, State}|
    {{back, work()}, State}|
    {stop, State} when
      State :: state().
