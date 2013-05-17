

-module(wrf_job).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-include_lib("flow/include/flow.hrl").
-export([run_job/1, test_job/0, detect_wrf_real/1, detect_wrf_build/1]).

%
%  Required inputs in the Cfg object
%
%    - wps_nl_template: a WPS namelist (as parsed by nllist:parse/1)
%    - wrf_nl_template: a WRF namelist (as parsed by nllist:parse/1)
%    - wrf_root_dir: the directory of the WRF installation to be used
%    - grib_files: list of GRIB files for use by ungrib.exe
%    - vtable_file: WPS-directory relative name of the Vtable file to use with ungrib.exe
%

test_job() ->

    inets:start(),
    stor:start(),
    
    WRFDir = "/home/martin/Projects/wrf-fire",
    WPSTempl = nllist:parse(filename:join(WRFDir, "WPS/namelist.wps")),
    WRFTempl = nllist:parse(filename:join(WRFDir, "WRFV3/run/namelist.input")),
    Cfg1 = wrf_nl:read_config(WRFTempl),

    NLSpec = wrf_reg:create_profile_from_reg(filename:join(WRFDir, "WRFV3/Registry"),
					     vanilla_wrf_v34),

    Cfg = plist:update_with([ {wrf_root_dir, WRFDir},
			      {wrf_build_type, detect_wrf_build(WRFDir)},
			      {wps_exec_dir, "/home/martin/Temp/wps_temp_anjk4378"},
			      {wrf_exec_dir, "/home/martin/Temp/wrf_temp_anjk4378"},
			      {wrf_exec_method, serial_local},
			      {wps_nl_template, WPSTempl},
			      {wrf_nl_template, WRFTempl},
			      {nl_spec, NLSpec},
			      {wrf_from, {{2013, 5, 1}, {0, 0, 0}}},
			      {wrf_to, {{2013, 5, 1}, {0, 30, 0}}},
			      {grib_interval_seconds, 1800},
			      {vtable_file, "ungrib/Variable_Tables/Vtable.NAM"},
			      {grib_sources, [rnrs_nam218]} ],
			    
			      Cfg1),

    % delete the test dir
    os:cmd(["rm -rf ", plist:getp(wps_exec_dir, Cfg)]),
    os:cmd(["rm -rf ", plist:getp(wrf_exec_dir, Cfg)]),

    run_job(Cfg).


run_job(Cfg) ->

    % FIXME: this is very basic, retrieve_grib_files needs to go into the retr module
    R = lists:nth(1, plist:getp(grib_sources, Cfg)),
    {{WPSFrom, WPSTo}, GribFiles} = retrieve_grib_files(R, plist:getp(wrf_from, Cfg), plist:getp(wrf_to, Cfg)),

    % given the limits in the GRIB files, update the configuration
    Cfg2 = plist:update_with([{grib_files, GribFiles}, {wps_from, WPSFrom}, {wps_to, WPSTo}], Cfg),

    % construct WRF and WPS namelists
    Cfg3 = make_namelists(Cfg2),

    % plan & execute WPS job
    Plan = wps_exec:make_exec_plan(Cfg3),
    io:format("wps_exec plan has ~p steps.~n", [plan:count_steps(Plan)]),

    PID = plan_runner:execute_plan(Plan),
    case plan_runner:wait_for_plan(PID) of
     	{success, _Log} ->
     	    prep_wrf(Cfg3);
	{failure, _MFA, Text, _R, _Log} ->
	    io:format("error during plan execution [~p]~n", [lists:flatten(Text)]),
	    {failure, Text}
    end.


prep_wrf(Cfg) ->

    % Make an execution plan and run it (this does everything except submitting the wrf_job
    Plan = wrf_prep:make_exec_plan(Cfg),
    io:format("~p~n", [Plan]),
    
    PID = plan_runner:execute_plan(Plan),
    case plan_runner:wait_for_plan(PID) of
	{success, _Log} ->
	    io:format("success.~n"),
	    run_wrf(plist:getp(wrf_exec_method, Cfg), Cfg);
	{failure, _MFA, Text, _R, _Log} ->
	    io:format("error during plan execution [~p]~n", [lists:flatten(Text)]),
	    {failure, Text}
    end.


run_wrf(serial_local, Cfg) ->
    Dir = plist:getp(wrf_exec_dir, Cfg),
    PID = exmon:run(filename:join(Dir, "wrf.exe"), [], [self()]),
    {ok, D} = file:open(filename:join(Dir, "wrf_output.log"), [write]),
    case wait_for_wrf_completion(PID, D) of
	success ->
	    post_wrf(Cfg);
	{failure, Text} ->
	    io:format("error during execution of wrf.exe [~p]~n", [lists:flatten(Text)]),
	    {failure, Text}
    end.


wait_for_wrf_completion(PID, D) ->
    receive

	% messages from the exmon wrf monitor
	{line, L} ->
	    % a wrf message processor should be here to scan output, predict completion
	    % detect error conditions, etc.
	    file:write(D, L),
	    file:write(D, "\n"),
	    wait_for_wrf_completion(PID, D);
	{exit_detected, 0} ->
	    file:close(D),
	    success;
	{exit_detected, Code} ->
	    file:close(D),
	    {failure, io:format("wrf.exe exited with code ~p~n", [Code])};
	
	% messages from command interface
	kill_job ->
	    exmon:kill(PID),
	    wait_for_wrf_completion(PID, D)
    end.


post_wrf(_Cfg) ->
    io:format("Not implemented yet."),
    success.


make_namelists(Cfg) ->
    WPSNL = wps_nl:write_config(Cfg, plist:getp(wps_nl_template, Cfg)),
    WRFNL = wrf_nl:write_config(Cfg, plist:getp(wrf_nl_template, Cfg)),
    plist:update_with([ {wps_nl, WPSNL}, {wrf_nl, WRFNL} ], Cfg).


retrieve_grib_files(R, From, To) ->
    Dom = apply(R, domain, []),
    URLBase = apply(R, url_prefix, []),
    {ok, Cov, IDs} = apply(R, manifest, [From, To, 0]),
    {Cov, retrieve_grib_files(Dom, URLBase, IDs, [])}.


retrieve_grib_files(_Dom, _URL, [], List) ->
    List;

retrieve_grib_files(Dom, URLBase, [ID|IDs], List) ->
    case stor:check_exists(Dom, ID) of
	not_found ->
	    {success, F} = stor:ensure_location(Dom, ID),
	    io:format("Downloading [~p]~n", [URLBase ++ ID]),
	    {success, _} = network_tasks:http_sync_get(URLBase ++ ID, F),
	    retrieve_grib_files(Dom, URLBase, IDs, [F|List]);
	F ->
	    retrieve_grib_files(Dom, URLBase, IDs, [F|List])
    end.

    

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


detect_wrf_build(WRFRoot) ->
    WRFRunDir = filename:join(WRFRoot, "WRFV3/run"),
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
