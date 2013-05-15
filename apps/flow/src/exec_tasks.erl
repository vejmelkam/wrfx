

%
%  This entire module needs to be restructured to
%  - use processes for monitoring execution
%  - grab whatever corresponds to stdout or stderr or whatever we want to monitor
%    and read it line by line, while watching for specific strings and firing off notifications
%

-module(exec_tasks).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([run_scan_output/3,run_maybe_mpi/3]).



run_scan_output(InDir, Cmd, ScanString) ->
    Output = os:cmd(io_lib:format("cd ~p && ~p", [InDir, Cmd])),
    case string:str(Output, ScanString) of
	0 ->
	    {failure, io_lib:format("execution of [~p] failed with output~n**** OUTPUT FOLLOWS ****~n~p~n**** OUTPUT END ****.", [Cmd, Output])};
	_ ->
	    {success, io_lib:format("execution of [~p] succeeded.", [Cmd])}
    end.



run_maybe_mpi(InDir, Cmd, ScanString) ->
    Output = os:cmd(io_lib:format("cd ~p && ~p", [InDir, Cmd])),
    case string:str(Output, "starting wrf task") of
	0 ->
	    % the file was compiled with MPI, output is in rsl.error.000
	    {ok, B} = file:read_file(filename:join(InDir, "rsl.error.0000")),

	    % this is inefficient but this segment is not performance sensitive
	    D = binary_to_list(B);
	_ ->
	    D = Output
    end,
    case string:str(D, ScanString) of
	0 ->
	    {failure, io_lib:format("execution of [~p] failed with output~n**** OUTPUT FOLLOWS ****~n~p~n**** OUTPUT END ****.",
				    [Cmd, D])};
	_ ->
	    {success, io_lib:format("execution of [~p] succeeded.", [Cmd])}
    end.
	    
