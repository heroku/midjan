-module(midjan_test1).

-behaviour(midjan_module).

-export([run/1]).

run({X, List}) ->
    {next, {X, List ++ [?MODULE]}}.
