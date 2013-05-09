

-module(exec_tasks).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([run_scan_output/3]).



run_scan_output(InDir, Cmd, ScanString) ->
    Output = os:cmd(io_lib:format("cd ~p && ~p", [InDir, Cmd])),
    case string:str(Output, ScanString) of
	0 ->
	    {failure, io_lib:format("execution of [~p] failed with output~n**** OUTPUT FOLLOWS ****~n~p~n**** OUTPUT END ****.", [Cmd, Output])};
	_ ->
	    {success, io_lib:format("execution of [~p] succeeded.", [Cmd])}
    end.
