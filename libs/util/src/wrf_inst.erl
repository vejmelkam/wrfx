
-module(wrf_inst).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([detect_build_target/1, detect_build_type/1]).
-include_lib("flow/include/flow.hrl").

detect_build_target(WRFRoot) ->
    FileList = [ "run/wrf.exe", "run/real.exe" ],
    P = #plan{id = check_wrf_installation,
	      tasks = [ {filesys_tasks, file_exists, [ filename:join(WRFRoot, F) ] } || F <- FileList ]},
    L = plan_logger:start(stdio),
    PID = plan_runner:execute_plan(P, [L]),
    case plan_runner:wait_for_plan(PID) of
	success ->
	    real_target;
	failure ->
	    unknown_target
    end.


detect_build_type(WRFRoot) ->
    WRFRunDir = filename:join(WRFRoot, "run"),
    Cmd = io_lib:format("nm ~p/wrf.exe", [WRFRunDir]),
    Output = os:cmd(Cmd),
    case string:str(Output, "wrf") of
	0 ->
	    failed;
	_ ->
	    case string:str(Output, "mpi_finalize") of
		0 ->
		    no_mpi;
		_ ->
		    with_mpi
	    end
    end.
