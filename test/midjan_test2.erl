-module(midjan_test2).

-behaviour(midjan_module).

-export([run/1]).

run({go_back, List}) ->
    {{back, midjan_test1}, {went_back, List ++ [?MODULE]}};
run({X, List}) ->
    {next, {X, List ++ [?MODULE]}}.
