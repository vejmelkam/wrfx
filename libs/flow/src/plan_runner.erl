
%% @doc
%% A simple plan runner that keeps executing a plan until there are
%% no more tasks or until a task returns a failure code.
%% 
%% Plans are always executed in a fresh process.
%% @end


-module(plan_runner).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([execute_plan/2, ping_planner/1, wait_for_plan/1]).
-include("include/flow.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc This function checks if a planner at the given PID is live.
%% @spec ping_planner(PID::pid()) -> success|no_reply
ping_planner(PID) ->
    PID ! {ping, self()},
    receive
	{pong, PID} ->
	    success
    after 1000 ->
	    no_reply
    end.


%% @doc This function blocks until a plan is complete, the result of the plan
%%      is returned.
%% @spec wait_for_plan(PID::pid()) -> {plan_complete, pid(), term()}
wait_for_plan(PID) ->
    receive
	{plan_complete, PID, Result} ->
	    Result
    end.
    

%% @doc Executes a plan asynchronously, returns immediately with
%%      the pid of the running plan.  Sends messages about plan progress to
%%      monitors in list M.
%% @spec execute_plan(plan(), [pid()]) -> pid()
execute_plan(#plan{tasks=T}, Monitors) ->
    S = self(),
    spawn(fun () -> S ! {plan_complete, self(), execute_plan_internal(T, Monitors)} end).


execute_plan_internal([], Monitors) ->
    msg_router:multicast({self(), success}, Monitors),
    {success, []};
execute_plan_internal([MFA={M,F,A}|Rest], Monitors) ->
    S = self(),
    PID = spawn(fun () -> S ! {async_task_done, self(), apply(M, F, A)} end),
    wait_and_check_messages(PID, MFA, Rest, Monitors).


wait_and_check_messages(PID, MFA, Rest, Monitors) ->
    receive
	{async_task_done, PID, {success, Text}} ->
	    msg_router:multicast({self(), task_done, MFA, Text}, Monitors),
	    execute_plan_internal(Rest, Monitors);
	{async_task_done, PID, {failure, Error}} ->
	    msg_router:multicast({self(), failure, MFA, Error}, Monitors),
	    {failure, Error};
	{ping, From} ->
	    From ! {pong, self()},
	    wait_and_check_messages(PID, MFA, Rest, Monitors);
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
		      [ {tasks_fsys, create_dir, [D]},
			[ {tasks_fsys, create_symlink, [filename:join(S,F), filename:join(D,F)] } || F <- Fs ] ])},

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
    
    P = #plan{id=test_async_plan, tasks = [ {tasks_util, sleep_ms, [1000]} ]},
    PID = execute_plan(P),
    success = ping_planner(PID),
    wait_for_plan(PID).

-endif.
