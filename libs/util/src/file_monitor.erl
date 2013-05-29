

%% @doc
%% A portable file monitoring service, which monitors a file for
%% newly appended lines and sends a {line, L} message to any
%% processes that were listed in the Monitors list in {@link start/2}.
%%
%% Note: NFS mounted drives manifest significant delay between the file
%% write and the moment this monitor detects the file change.
%%


-module(file_monitor).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/2, stop/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @doc
%% Starts the file monitor for file F. New line messages will be
%% sent to all processes in the Monitors list.  Returns the pid of
%% the file monitor.
%% @spec start(F::string(), Monitors::[pid()]) -> pid()
start(F, Monitors) ->
    spawn(fun () -> start_monitor(F, Monitors) end).


%% @doc
%% Stops the file monitor.  This causes {eof, From} messages to be sent
%% to all the monitors, where From is the process that invoked {@link stop/1}.
%% @spec stop(PID::pid) -> ok
stop(PID) ->
    PID ! {stop, self()},
    ok.


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
	    msg_router:multicast({line, L}, M),
	    try_read_line(continue, D, M);
	{error, R} ->
	    msg_router:multicast({error, R}, M),
	    file:close(D)
    end;
try_read_line(stop, D, _M) ->
    file:close(D).


check_for_messages(M) ->
    receive
	{stop, From} ->
	    msg_router:multicast({eof, From}, M),
	    stop;
	Msg ->
	    io:format("file_monitor: unexpected message [~p]~n", [Msg]),
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
