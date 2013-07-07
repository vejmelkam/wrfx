


%% @doc
%% A logging process that routes incoming messages to a given
%% list of targets.
%%

-module(logd).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([open/1, close/1, message/2, message/3]).


%% @doc Starts a logger that logs to the targets given in a list.
%% A target is either stdio or a filename string.
%% @spec open([stdio|string()]) -> pid()
open(Ts) ->
    Devs = lists:map(fun open_target/1, Ts),
    spawn(fun () -> logd_msg_loop(Devs) end).

%% @doc Terminates the log process and closes all targets.
%% @spec close(pid()) -> ok
close(PID) ->
    PID ! {self(), close},
    ok.

%% @doc Sends a message to the logger, which is routed to all targets.
%% @spec message(string(), pid()) -> ok
message(M, PID) ->
    PID ! {self(), msg, M},
    ok.

%% @doc Sends a message to the logger, which is routed to all targets.
%% @spec message(string(), [term()], pid()) -> ok
message(M, A, PID) ->
    PID ! {self(), msg, io_lib:format(M, A)},
    ok.


logd_msg_loop(Ds) ->
    receive
	{_PID, msg, M} ->
	    lists:map(fun (D) -> write_message(M, D) end, Ds),
	    logd_msg_loop(Ds);
	{_PID, close} ->
	    lists:map(fun (D) -> write_message("log closed", D) end, Ds),
	    lists:foreach(fun close_target/1, Ds)
    end.


write_message(M, stdio) ->
    T = atime:dt_ts_str(calendar:local_time()),
    io:fwrite("[~s] ~s.~n", [T, lists:flatten(M)]);
write_message(M, D) ->
    T = atime:dt_ts_str(calendar:local_time()),
    file:write(D, io_lib:format("[~s] ~s.~n", [T, lists:flatten(M)])).


open_target(stdio) ->
    stdio;
open_target(F) ->
    {ok, D} = file:open(F, [write]),
    D.


close_target(stdio) ->
    ok;
close_target(D) ->
    file:close(D).

