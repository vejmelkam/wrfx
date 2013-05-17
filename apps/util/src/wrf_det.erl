
%
%  A module that analyses a WRF installation in use.
%
%   - currently supplies detection of MPI/serial compilation
%

-module(wrf_det).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([detect_wrf_real/1, detect_build_type/1]).
-include_lib("../../flow/include/flow.hrl").

detect_wrf_real(WRFRoot) ->

    FileList = [ "WRFV3/run/wrf.exe", "WRFV3/run/real.exe" ],
    P = #plan{id = check_wrf_installation,
	      tasks = [ {filesys_tasks, file_exists, [ filename:join(WRFRoot, F) ] } || F <- FileList ]},
    PID = plan_runner:execute_plan(P),
    case plan_runner:wait_for_plan(PID) of
	{success,_} ->
	    have_wrf_real;
	_ ->
	    no_wrf_real
    end.


detect_build_type(WRFRoot) ->
    WRFRunDir = filename:join(WRFRoot, "WRFV3/run"),
    Cmd = io_lib:format("nm ~p/wrf.exe", [WRFRunDir]),
    Output = os:cmd(Cmd),
    io:format(Output),
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
