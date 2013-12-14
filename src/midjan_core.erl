-module(midjan_core).

-export([start/2]).

-type opts() :: [opt()]|[].
-type opt() :: {ordered, [midjan_module:work()]}|
               {translator, midjan_translator:translator()}|
               {aftereach, midjan_module:work()}.

-export_type([opts/0,
              opt/0]).

-record(state, {
          aftereach :: module()|undefined,
          ordered :: [module()],
          path :: [module()]|[],
          translator :: module()|undefined
         }).

-spec start(any(), opts()) ->
                   {done, any()}|
                   no_return().
start(Client, Opts) ->
    decide_and_execute(Client, next, undefined, init_state(Opts, #state{})).

execute(Client, ModuleToRun, State) ->
    {NextAction, Client1, State1} = run_module(Client, ModuleToRun, State),
    {NextAction1, Client2, State2} = maybe_run_after(Client1, NextAction, State1),
    decide_and_execute(Client2, NextAction1, ModuleToRun, State2).

maybe_run_after(Client, NextAction, #state{aftereach=undefined}=State) ->
    {NextAction, Client, State};
maybe_run_after(Client, _, #state{aftereach=Module}=State) ->
    run_module(Client, Module, State).

decide(stop, _LastModule, _State) ->
    stop;
decide(next, _LastModule, #state{path=[]}) ->
    stop;
decide(next, _LastModule, #state{path=[NextModule|Rest]}=State) ->
    %% Continue running
    {NextModule, State#state{path=Rest}};
decide({back, NextModule}, _LastModule, #state{ordered=Ordered}=State) ->
    %% In this case midjan will rewind back, and change the path so that it will rerun all 
    %% the modules after NextModule. This has an issue when a developer has the same module
    %% a few times in the ordered list. That can be fixed by keeping the index.
    NewPath = lists:dropwhile(fun(M) ->
                                      if M =:= NextModule -> true;
                                         true -> false
                                      end
                              end, Ordered),
    {NextModule, State#state{path=NewPath}}.

decide_and_execute(Client, NextAction, LastModule, State) ->
    case decide(NextAction, LastModule, State) of
        stop ->
            {done, Client};
        {NextModule, State1} ->
            execute(Client, NextModule, State1)
    end.

run_module(Client, ModuleToRun, #state{translator=undefined}=State) ->
    Res = ModuleToRun:run(Client),
    handle_module_output(Res, State);
run_module(Client, ModuleToRun, #state{translator=Translator}=State) ->
    Res = Translator:run(ModuleToRun, Client),
    handle_module_output(Res, State).

handle_module_output({next, Client}, State) ->
    {next, Client, State};
handle_module_output({{back, _NextModule}=Cmd, Client}, State) ->
    {Cmd, Client, State};
handle_module_output({stop, Client}, State) ->
    {stop, Client, State}.

init_state([], State) ->
    State;
init_state([{translator, Module}|Rest], State) ->
    init_state(Rest, State#state{translator=Module});
init_state([{ordered, Modules}|Rest], State) ->
    init_state(Rest, State#state{ordered=Modules,
                                 path=Modules});
init_state([{aftereach, Module}|Rest], State) ->
    init_state(Rest, State#state{aftereach=Module}).
