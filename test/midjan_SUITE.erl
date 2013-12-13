-module(midjan_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, midjan_ordered}
    ].

groups() ->
    [
     {midjan_ordered, [], [full_flow,
                           after_each,
                           go_back,
                           translator
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

after_each(Config) ->
    {done, Results} = midjan_core:start({full_flow, []}, [{ordered, [midjan_test1,
                                                                     midjan_test2]},
                                                          {aftereach, midjan_test3}]),
    {_, [midjan_test1, midjan_test3, midjan_test2, midjan_test3]} = Results,
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
