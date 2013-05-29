

%
%  This module provides task execution services with
%  
%  - reading in stdout from the process, or
%  - reading in any output file that the process generates
%  - capture the exit code of the process
%

-module(tasks_exec).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([execute/2]).


execute(Cmd, Opts) ->
    InDir = plist:getp(in_dir, Opts),
    Monitors = plist:getp(monitors, Opts, []),
    StoreFile = plist:getp(store_output_to, Opts),
    Capture = plist:getp(output_type, Opts),
    ExtraOP = plist:getp(op_args, Opts, []),
    ExitCheck = plist:getp(exit_check, Opts, exit_code),
    case Capture of
	stdout ->
	    execute_capture_stdout(Cmd, InDir, Monitors, StoreFile, ExtraOP, ExitCheck);
	_ ->
	    execute_capture_file(Cmd, InDir, Monitors, Capture, StoreFile, ExtraOP, ExitCheck)
    end.



execute_capture_stdout(Cmd, InDir, Monitors, StoreFile, ExtraOP, ExitCheck) ->
    % starts file sink which copies stdout of program to desired storage
    FS = sink_file:start(StoreFile),

    % run the external command and monitor execution until done
    XPID = exproc:run_and_monitor(Cmd, [{cd, InDir}|ExtraOP], [FS|Monitors]),
    C = wait_for_completion(XPID),

    % close the file sink
    sink_file:send_eof(FS),
 
    evaluate_result(ExitCheck, C, StoreFile, Cmd).


execute_capture_file(Cmd, InDir, Monitors, CaptureFile, StoreFile, ExtraOP, ExitCheck) ->
    % starts a file monitor of the file to which program outputs
    FM = file_monitor:start(CaptureFile, Monitors),

    % run the external command and monitor execution until done
    XPID = exproc:run(Cmd, [{cd, InDir}|ExtraOP], []),
    C = wait_for_completion(XPID),

    % close the file monitor
    file_monitor:stop(FM),
    file:rename(CaptureFile, StoreFile),
    evaluate_result(ExitCheck, C, StoreFile, Cmd).



wait_for_completion(XPID) ->
    receive
	kill ->
	    exproc:kill9(XPID),
	    wait_for_completion(XPID);
	{XPID, exit_detected, S} ->
	    S
    end.


evaluate_result(exit_code, 0, _F, Cmd) ->
    {success, io_lib:format("success executing ~s~n", [Cmd])};
evaluate_result(exit_code, C, _F, Cmd) ->
    {failure, io_lib:format("execution of ~s failed with exit code ~s~n", [Cmd, C])};
evaluate_result({scan_for, S}, C, F, Cmd) ->
    case scan_file(S, F) of
	found ->
	    {success, io_lib:format("command [~s] completed succesfully with exit code ~p.~n", [Cmd, C])};
	not_found ->
	    {failure, io_lib:format("string [~s] was not found in the output of [~s], exit_code is [~p]~n", [S, Cmd, C])}
    end.


scan_file(S, F) ->
    case file:open(F, [read]) of
	{ok, D} ->
	    R = scan_stream(S, D),
	    file:close(D),
	    R;
	{error, _} ->
	    not_found
    end.


scan_stream(S, D) ->
    case file:read_line(D) of
	{ok, L} ->
	    case string:str(L, S) of
		0 ->
		    scan_stream(S, D);
		_ ->
		    found
	    end;
	_ ->
	    not_found
    end.
					  
