

%% @doc
%% A plan logger is a plan monitor that logs everything that happens
%% to a logd process.
%%

-module(plan_logger).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/1, stop/1]).


start(PID) ->
    spawn(fun () -> plan_logger_loop(PID) end).


plan_logger_loop(PID) ->
    receive
	{_PID, task_start, {M,F,A}} ->
	    logd:message("[task init] ~s:~s args ~p", [M, F, A], PID),
	    plan_logger_loop(PID);
	{_PID, task_done, Text} ->
	    logd:message("[task complete] ~s", [Text], PID),
	    plan_logger_loop(PID);
	{_PID, failure, Error} ->
	    logd:message("[FAILED] ~s", [Error], PID);
	{_PID, success} ->
	    logd:message("[SUCCESS] plan complete", PID);
	{_PID, stop} ->
	    logd:message("[PLAN LOGGER STOPPED]", PID)
    end.


stop(PID) ->
    PID ! {self(), stop},
    ok.

