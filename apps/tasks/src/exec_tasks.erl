

%
%  This module provides task execution services with
%  
%  - reading in stdout from the process, or
%  - reading in any output file that the process generates
%  - capture the exit code of the process
%

-module(exec_tasks).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([execute/2]).


execute(Cmd, Opts) ->
    InDir = plist:getp(in_dir, Opts),
    Monitors = plist:getp(monitors, Opts, []),
    StoreFile = plist:getp(store_to, Opts),
    Capture = plist:getp(output_type, Opts),
    ExtraOP = plist:getp(op_args, Opts, []),
    ExitCheck = plist:getp(exit_check, Opts, exit_code),

    % starts file monitors and file sinks as needed
    {FM, FS, M2} = handle_capture_request_start(Capture, StoreFile, Monitors),
    M3 = case ExitCheck of
	     {scan_for, _S} ->
		 [self()|M2];
	     _ ->
		 M2
	 end,
    XPID = exmon:run(Cmd, [{cd, InDir}|ExtraOP], M3),
    R = wait_for_completion(XPID, ExitCheck, Cmd, failure),
    % closes whatever was started by request_start above
    handle_capture_request_end(Capture, StoreFile, FM, FS),
    R.


handle_capture_request_start(stdout, FName, M) ->
    FS = file_sink:start(FName),
    {undefined, FS, [FS|M]};
handle_capture_request_start(FName1, _FName2, M) ->
    FM = fmon:start(FName1),
    {FM, undefined, M}.



handle_capture_request_end(stdout, _FName, undefined, FS) ->
    file_sink:stop(FS);
handle_capture_request_end(FName1, FName2, FM, undefined) ->
    fmon:stop(FM),
    file:rename(FName1, FName2).




wait_for_completion(XPID, ExitCheck, Cmd, Status) ->
    receive
	kill ->
	    exmon:kill9(XPID),
	    wait_for_completion(XPID, ExitCheck, Cmd, Status);
	{XPID, line, L} ->
	    {scan_for, Str} = ExitCheck,
	    case string:str(L, Str) of
		0 ->
		    wait_for_completion(XPID, ExitCheck, Cmd, Status);
		_ ->
		    wait_for_completion(XPID, ExitCheck, Cmd, success)
	    end;
	{XPID, exit_detected, S} ->
	    io:format("detected exit of [~p]~n", [Cmd]),
	    exec_prog_eval_result(ExitCheck, S, Status, Cmd)
    end.

exec_prog_eval_result(exit_code, 0, _Status, Cmd) ->
    {success, io_lib:format("success executing ~p~n", [Cmd])};
exec_prog_eval_result(exit_code, Code, _Status, Cmd) ->
    {failure, io_lib:format("execution of ~p failed with exit code ~p~n", [Cmd, Code])};
exec_prog_eval_result({scan_for, S}, Code, failure, Cmd) ->
    {failure, io_lib:format("string [~p] was not found in the output of [~p], exit_code is [~p]~n",
			    [S, Cmd, Code])};
exec_prog_eval_result({scan_for, _S}, _Code, success, Cmd) ->
    {success, io_lib:format("command [~p] completed succesfully.~n", [Cmd])}.
