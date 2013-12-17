-module(midjan_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, midjan_ordered}
    ].

groups() ->
    [
     {midjan_ordered, [], [full_flow,
                           go_back,
                           translator,
                           before_fun,
                           after_fun
                          ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(_, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% ----------
%% TEST CASES
%% ----------
full_flow(Config) ->
    {done, Results} = midjan_core:start({full_flow, []}, [{ordered, [midjan_test1,
                                                                     midjan_test2]}]),
    {_, [midjan_test1, midjan_test2]} = Results,
    Config.

go_back(Config) ->
    {done, Results} = midjan_core:start({go_back, []}, [{ordered, [midjan_test1,
                                                                   midjan_test2,
                                                                   midjan_test3]}]),
    {_, [midjan_test1, midjan_test2, midjan_test1, midjan_test2, midjan_test3]} = Results,
    {done, Results1} = midjan_core:start({go_back2, []}, [{ordered, [midjan_test1,
                                                                     midjan_test2,
                                                                     midjan_test3]}]),
    {_, [midjan_test1, midjan_test2, midjan_test3,
         midjan_test1, midjan_test2, midjan_test3]} = Results1,
    Config.

translator(Config) ->
    {done, Results} = midjan_core:start({translator, []}, [{ordered, [midjan_test1,
                                                                      midjan_test2,
                                                                      midjan_test3]},
                                                           {translator, midjan_translator}
                                                          ]),
    {translated, [midjan_test1, midjan_test2, midjan_test3]} = Results,
    Config.

before_fun(Config) ->
    {done, Results} = midjan_core:start({before_fun, []}, [{ordered, [midjan_test1,
                                                                      midjan_test2,
                                                                      midjan_test3
                                                                     ]},
                                                           {before_each, fun hook_test/2}
                                                          ]),
    {before_fun, [{hook, midjan_test1}, midjan_test1, 
                  {hook, midjan_test2}, midjan_test2, 
                  {hook, midjan_test3}, midjan_test3]} = Results,
    Config.

after_fun(Config) ->
        {done, Results} = midjan_core:start({after_fun, []}, [{ordered, [midjan_test1,
                                                                         midjan_test2,
                                                                         midjan_test3
                                                                        ]},
                                                              {after_each, fun hook_test/2}
                                                             ]),
    {after_fun, [midjan_test1, {hook, midjan_test1},
                  midjan_test2, {hook, midjan_test2},
                  midjan_test3, {hook, midjan_test3}]} = Results,
    Config.

%% Internal
hook_test({X, List}, Module) ->
    {X, List ++ [{hook, Module}]}.
