

-module(plan_logger).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/1]).


%% @doc Starts a plan logger that either logs to stdio if argument is stdio or to a file
%%      if the filename is a string.
%% @spec start(D:: stdio|string()) -> pid()
start(stdio) ->
    spawn(fun () -> logger_msg_loop(stdio) end);
start(Filename) ->
    {ok, D} = file:open(Filename, [write]),
    spawn(fun () -> logger_msg_loop(D) end).


logger_msg_loop(Dev) ->
    receive
	{_PID, task_done, MFA, Text} ->
	    log_message(Dev, task_done, MFA, Text),
	    logger_msg_loop(Dev);
	{_PID, failure, MFA, Text} ->
	    log_message(Dev, failure, MFA, Text),
	    file:close(Dev);
	{_PID, success} ->
	    log_message(Dev, plan_complete, [], []),
	    file:close(Dev)
    end.

		   
log_message(D, task_done, _MFA, Text) ->
    log_string(D, io_lib:format("Task complete: ~s.~n", [lists:flatten(Text)]));
log_message(D, failure, {M, F, _A}, Text) ->
    log_string(D, io_lib:format("Plan execution failed at task ~p:~p with error ~s.~n", [M, F, lists:flatten(Text)]));
log_message(D, plan_complete, [], []) ->
    log_string(D, "Plan executed successfully.~n").

log_string(stdio, S) ->
    io:format(S);
log_string(Dev, S) ->
    file:write(Dev, S).
