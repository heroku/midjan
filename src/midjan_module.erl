%%% @doc Callback / behaviour module.
%%% Defines a piece of work to be done, and the flow required when said
%%% work is done.
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
