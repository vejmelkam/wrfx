
%
%  A first shot at a state machine, which executes plans.
%  Plans contain tasks which are atomic units that either
%  succeed or fail.
%

-module(plan_runner).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([execute_plan/1, ping_planner/1, wait_for_plan/1]).
-include("include/flow.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

    


ping_planner(PID) ->
    PID ! {ping, self()},
    receive
	{pong, PID} ->
	    success
    after 1000 ->
	    no_reply
    end.


wait_for_plan(PID) ->
    receive
	{plan_complete, PID, Result} ->
	    Result
    end.
    

execute_plan(#plan{tasks=T}) ->
    S = self(),
    spawn(fun () -> S ! {plan_complete, self(), execute_plan(T, [])} end).


execute_plan([], Log) ->
    {success, lists:reverse(Log)};
execute_plan([MFA={M,F,A}|Rest], Log) ->
    S = self(),
    PID = spawn(fun () -> S ! {async_task_done, self(), apply(M, F, A)} end),
    wait_for_result(PID, MFA, Rest, Log).


wait_for_result(PID, MFA, Rest, Log) ->
    receive
	{async_task_done, PID, {success, Text}} ->
	    execute_plan(Rest, [Text|Log]);
	{async_task_done, PID, {failure, Text}} ->
	    {failure, MFA, Text, Rest, lists:reverse(Log)};
	{ping, From} ->
	    From ! {pong, self()},
	    wait_for_result(PID, MFA, Rest, Log);
	BadMsg ->
	    io:format("Unexpected message [~p]~n", [BadMsg])
    end.



-ifdef(TEST).

execute_clone_dir_plan_test() ->

    S = "/home/martin/Temp/t1",
    D = "/home/martin/Temp/t2",
    Fs = ["file1", "file2", "readme"],

    P = #plan{id=clone_dir_test_exec,
	      tasks=lists:flatten(
		      [ {filesys_tasks, create_dir, [D]},
			[ {filesys_tasks, create_symlink, [filename:join(S,F), filename:join(D,F)] } || F <- Fs ] ])},

    PID = execute_plan(P),
    case wait_for_plan(PID) of
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


async_exec_test() ->
    
    P = #plan{id=test_async_plan, tasks = [ {util_tasks, sleep_ms, [1000]} ]},
    PID = execute_plan(P),
    success = ping_planner(PID),
    wait_for_plan(PID).

-endif.