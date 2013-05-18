
%
%  Execution and monitoring module for external processes.
%  Uses open_port/2 to establish communication with an external
%  program.
%


-module(exmon).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([run/3, kill/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


kill(PID) ->
    PID ! send_kill.


run(ExFile, Opts, Monitors) ->
    spawn(fun() -> run_and_monitor(ExFile, Opts, Monitors) end).


run_and_monitor(ExFile, Opts, Monitors) ->
    P = open_port({spawn_executable, ExFile}, [{line, 5000}, exit_status, use_stdio | Opts]),
    monitor_process(P, Monitors).
    

monitor_process(P, Monitors) ->
    receive
	send_kill ->
	    {os_pid, OsPid} = erlang:port_info(P, os_pid),
	    os:cmd(io_lib:format("kill -9 ~p", [OsPid])),
	    monitor_process(P, Monitors);
	{_From, {data, {eol, Line}}} ->
	    router:multicast({line, Line}, Monitors),
	    monitor_process(P, Monitors);
	{_From, {exit_status, S}} ->
	    router:multicast({exit_detected, S}, Monitors);
	M ->
	    io:format("exmon: unexpected message ~p~n", [M]),
	    monitor_process(P, Monitors)
    end.



-ifdef(TEST).

simple_test() ->
    run("/bin/echo", ["test_string"], [self()]),
    receive
	Msg ->
	    ?assert(Msg =:= {line, "test_string"})
    end,
    receive
	Msg2 ->
	    ?assert(Msg2 =:= {exit_detected, 0})
    end.


kill_test() ->
    PID = run("/bin/sleep", ["5"], [self()]),
    kill(PID),
    receive
	Msg ->
	    ?assert(element(1, Msg) =:= exit_detected)
    end.


-endif.
    
