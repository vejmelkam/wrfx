
%
%  A first shot at a state machine, which executes plans.
%  Plans contain tasks which are atomic units that either
%  succeed or fail.
%

-module(plan_runner).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([execute_plan/1]).
-include("include/flow.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


execute_plan(#plan{tasks=Plan}) ->
    execute_plan(Plan, []).


execute_plan([], Log) ->
    {success, lists:reverse(Log)};
execute_plan([MFA|Rest], Log) ->
    io:format("have argument: ~p~n", [MFA]),
    {M, F, A} = MFA,
    case apply(M, F, A) of
	{success, Text} ->
	    execute_plan(Rest, [Text|Log]);
	{failure, Text} ->
	    {failure, MFA, Text, Rest, lists:reverse(Log)}
    end.

    

-ifdef(TEST).

execute_clone_dir_plan_test() ->
    A = [ {src_dir, "/home/martin/Temp/t1"},
	  {dst_dir, "/home/martin/Temp/t2"},
	  {with_files, ["file1", "file2", "readme"]} ],
    P = clone_dir_planner:make_plan(A),
    case execute_plan(P) of
	{success, Log} ->
	    file:write_file("planner.log", io_lib:format("success~n~p~n", [Log]));
	{failure, MFA, Text, _R, Log} ->
	    file:write_file("planner.log", io_lib:format("failure-at~n~p~nwith text~p~n~p~n", [MFA, Text, Log]))
    end,

    ?assert(filelib:is_dir("/home/martin/Temp/t2")),
    ?assert(filelib:is_regular("/home/martin/Temp/t2/file1")),
    ?assert(filelib:is_regular("/home/martin/Temp/t2/file2")),
    ?assert(filelib:is_regular("/home/martin/Temp/t2/readme")),
    os:cmd("rm -rf /home/martin/Temp/t2").
    

-endif.
