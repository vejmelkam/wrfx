

%% @doc A file sink: a process that opens a file and waits for messages in the form 
%%      {From, line, L} and writes those to the file.  When it receives {From, eof},
%%      the process closes the file and terminates.
%%


-module(sink_file).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/1, send_line/2, send_eof/1]).


%% @doc Opens the target file and starts the file sink receive loop.
%% @spec start(F::string()) -> pid()
start(F) ->
    {ok, D} = file:open(F, [write]),
    spawn(fun () -> sink_file_loop(D) end).


%% @doc Sends an eof message to the file sink, which closes the file and terminates the process.
%% @spec send_eof(PID::pid()) -> ok
send_eof(PID) ->
    PID ! {self(), eof},
    ok.


%% @doc Sends a line to the file sink, which is written using {@link file:write/2} to the open device.
%%      A new line is appended to the file after the above line is written.
%% @spec send_line(PID::pid(), L::iodata()) -> ok
send_line(PID, L) ->
    PID ! {self(), line, L},
    ok.


sink_file_loop(D) ->
    receive
	{_From, line, L} ->
	    file:write(D, L),
	    file:write(D, "\n"),
	    sink_file_loop(D);
	{_From, eof} ->
	    file:close(D)
    end.
