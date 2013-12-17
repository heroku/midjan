-module(midjan_core).

-export([start/2]).

-type opts() :: [opt()]|[].
-type opt() :: {ordered, [midjan_module:work()]}|
               {translator, midjan_translator:translator()}|
               {after_each, hook_fun()}|
               {before_each, hook_fun()}.
-type hook_fun() :: fun((midjan_module:state(), midjan_module:work()) ->
                               midjan_module:state()).

-export_type([opts/0,
              opt/0,
              hook_fun/0]).

-record(state, {
          after_each :: hook_fun()|undefined,
          before_each :: hook_fun()|undefined,
          ordered :: [module()],
          path :: [module()]|[],
          translator :: module()|undefined
         }).

-spec start(any(), opts()) ->
                   {done, any()}|
                   no_return().
start(Client, Opts) ->
    decide_and_execute(Client, next, init_state(Opts, #state{})).

execute(Client, ModuleToRun, State) ->
    Client1 = run_before(Client, ModuleToRun, State),
    {NextAction1, Client2, State1} = run_decision(Client1, ModuleToRun, State),
    Client3 = run_after(Client2, ModuleToRun, State1),
    decide_and_execute(Client3, NextAction1, State1).

run_before(Client, _, #state{before_each=undefined}) -> Client;
run_before(Client, NextModule, #state{before_each=BeforeEach}) ->
    BeforeEach(Client, NextModule).

run_after(Client, _, #state{after_each=undefined}) -> Client;
run_after(Client, LastModule, #state{after_each=AfterEach}) ->
    AfterEach(Client, LastModule).

decide(stop, _State) ->
    stop;
decide(next, #state{path=[]}) ->
    stop;
decide(next, #state{path=[NextModule|Rest]}=State) ->
    {NextModule, State#state{path=Rest}};
decide({back, NextModule}, #state{ordered=Ordered}=State) ->
    %% In this case midjan will rewind back, and change the path so that it will rerun all 
    %% the modules after NextModule. This has an issue when a developer has the same module
    %% a few times in the ordered list. That can be fixed by keeping the index.
    NewPath = lists:dropwhile(fun(M) ->
                                      if M =:= NextModule -> true;
                                         true -> false
                                      end
                              end, Ordered),
    {NextModule, State#state{path=NewPath}}.

decide_and_execute(Client, NextAction, State) ->
    case decide(NextAction, State) of
        stop ->
            {done, Client};
        {NextModule, State1} ->
            execute(Client, NextModule, State1)
    end.

run_decision(Client, ModuleToRun, #state{translator=undefined}=State) ->
    Res = ModuleToRun:run(Client),
    handle_module_output(Res, State);
run_decision(Client, ModuleToRun, #state{translator=Translator}=State) ->
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
init_state([{after_each, Fun}|Rest], State) ->
    init_state(Rest, State#state{after_each=Fun});
init_state([{before_each, Fun}|Rest], State) ->
    init_state(Rest, State#state{before_each=Fun}).
