-module(midjan_translator).

-export([run/2]).
    
run(ModuleToRun, {translator, State}) ->
    %% Sneaky changing the state here to
    %% to testing
    ModuleToRun:run({translated, State});
run(ModuleToRun, State) ->
    ModuleToRun:run(State).

