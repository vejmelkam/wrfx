

-module(exec_tasks).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([run_scan_output/2]).



run_scan_output(Cmd, ScanString) ->
    Output = os:cmd(Cmd),
    case string:str(ScanString, Output) of
	0 ->
	    {failure, io_lib:format("execution of [~p] failed with output~n**** OUTPUT FOLLOWS ****~n~p~n**** OUTPUT END ****.", [Cmd, Output])};
	_ ->
	    {success, io_lib:format("execution of [~p] succeeded.")}
    end.
