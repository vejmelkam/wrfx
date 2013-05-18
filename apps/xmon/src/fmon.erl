

%
%
%  A file monitoring service that watches a file and sends
%  out new available data (whenever an entire line is available)
%  to registered Monitors.
%
%


-module(fmon).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/2, stop/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


start(F, Monitors) ->
    spawn(fun () -> start_monitor(F, Monitors) end).


stop(PID) ->
    PID ! {stop, self()}.


start_monitor(F, Monitors) ->
    wait_for_file(continue, F, Monitors).


wait_for_file(continue, F, M) ->
    case filelib:is_file(F) of
	true ->
	    {ok, D} = file:open(F, [read]),
	    try_read_line(continue, D, M);
	false ->
	    wait_for_file(check_for_messages(M), F, M)
    end;
wait_for_file(stop, _F, _M) ->
    ok.


try_read_line(continue, D, M) ->
    case file:read_line(D) of
	eof ->
	    try_read_line(check_for_messages(M), D, M);
	{ok, L} ->
	    router:multicast({line, L}, M),
	    try_read_line(continue, D, M);
	{error, R} ->
	    router:multicast({error, R}, M),
	    file:close(D)
    end;
try_read_line(stop, D, _M) ->
    file:close(D).


check_for_messages(M) ->
    receive
	{stop, From} ->
	    router:multicast({eof, From}, M),
	    stop;
	Msg ->
	    io:format("fmon: unexpected message [~p]~n", [Msg]),
	    continue
    after
	500 ->
	    continue
    end.
	
    

-ifdef(TEST).

simple_test() ->
    PID = start("/etc/passwd", [self()]),
    stop(PID),
    read_all_in(self()).

read_all_in(MasterPID) ->
    receive
	{line, _L} ->
	    read_all_in(MasterPID);
	{eof, MasterPID} ->
	    ok
    end.
    
-endif.
