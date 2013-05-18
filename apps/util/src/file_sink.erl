

%
%
%


-module(file_sink).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/1, stop/1]).



start(F) ->
    {ok, D} = file:open(F, [write]),
    spawn(fun () -> file_sink_loop(D) end).


stop(PID) ->
    PID ! {eof, self()}.


file_sink_loop(D) ->
    receive
	{_PID, line, L} ->
	    file:write(D, L),
	    file:write(D, "\n"),
	    file_sink_loop(D);
	{_PID, eof, _From} ->
	    file:close(D)
    end.
