
%
%  Execution and monitoring module for external processes.
%  Uses open_port/2 to establish communication with an external
%  program.
%


-module(exproc).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([run_and_monitor/3, kill9/1, wait_for_completion/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


kill9(PID) ->
    PID ! kill9.


wait_for_completion(PID) ->
    receive
	{PID, exit_detected, S} ->
	    S
    end.


run_and_monitor(ExFile, Opts, Monitors) ->
    S = self(),
    Owner = case lists:member(S, Monitors) of
		true ->
		    dont_send;
		false ->
		    S
	    end,
    spawn(fun() -> run_and_monitor(ExFile, Opts, Owner, Monitors) end).


run_and_monitor(ExFile, Opts, Owner, Monitors) ->
    P = open_port({spawn_executable, ExFile}, [{line, 5000}, exit_status, use_stdio | Opts]),
    monitor_process(P, Owner, Monitors).
    

monitor_process(P, Owner, Monitors) ->
    receive
	kill9 ->
	    R = erlang:port_info(P, os_pid),
	    {os_pid, OsPid} = R,
	    os:cmd(io_lib:format("kill -9 ~p", [OsPid])),
	    monitor_process(P, Owner, Monitors);
	{_From, {data, {eol, Line}}} ->
	    msg_router:multicast({self(), line, Line}, Monitors),
	    monitor_process(P, Owner, Monitors);
	{_From, {exit_status, S}} ->
	    % Owner is always notified that the process exited
	    case Owner of
		dont_send ->
		    msg_router:multicast({self(), exit_detected, S}, Monitors);
		PID ->
		    msg_router:multicast({self(), exit_detected, S}, [PID|Monitors])
	    end;
	M ->
	    io:format("mon_exec: unexpected message ~p~n", [M]),
	    monitor_process(P, Owner, Monitors)
    end.



-ifdef(TEST).

simple_test() ->
    PID = run("/bin/echo", [{args, ["test_string"]}], [self()]),
    receive
	Msg ->
	    ?assert(Msg =:= {PID, line, "test_string"})
    end,
    wait_for_completion(PID).


kill_test() ->
    PID = run("/bin/sleep", [{args, ["10"]}], [self()]),
    kill9(PID),
    wait_for_completion(PID).


-endif.
    
