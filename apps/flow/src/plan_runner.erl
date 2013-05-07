
%
%  A first shot at a state machine, which executes plans.
%  Plans contain tasks which are atomic units that either
%  succeed or fail.
%

-module(plan_runner).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([execute_plan/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


execute_plan(Plan) ->
    execute_plan(Plan, []).


execute_plan([], Log) ->
    {success, [], lists:reverse(Log)};
execute_plan([MFA|Rest], Log) ->
    {M, F, A} = MFA,
    case apply(M, F, A) of
	{success, Text} ->
	    execute_plan(Rest, [Text|Log]);
	{failure, Text} ->
	    {failure, MFA, Text, Rest, lists:reverse(Log)}
    end.

    

-ifdef(TEST).

execute_clone_dir_plan() ->
    A = [ {src_dir, "/home/martin/Temp/t1"},
	  {dst_dir, "/home/martin/Temp/t2"},
	  {with_files, ["file1", "file2", "readme"]} ],
    P = clone_dir_planner:make_plan(A),
    R = execute_plan(P).

-endif.
