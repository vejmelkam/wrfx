

-module(wrf_job).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-include_lib("flow/include/flow.hrl").
-export([run_job/1, test_serial_job/0, test_mpi_job/0, detect_wrf_real/1, detect_wrf_build/1]).

%
%  Required inputs in the Cfg object
%
%    - wps_nl_template: a WPS namelist (as parsed by nllist:parse/1)
%    - wrf_nl_template: a WRF namelist (as parsed by nllist:parse/1)
%    - wrf_root_dir: the directory of the WRF installation to be used
%    - grib_files: list of GRIB files for use by ungrib.exe
%    - vtable_file: WPS-directory relative name of the Vtable file to use with ungrib.exe
%

test_serial_job() ->

    cfg:start(),
    inets:start(),
    stor:start(),
    
    WRFDir = cfg:get_conf(default_wrf),
    WPSDir = cfg:get_conf(default_wps),
    WPSTempl = nllist:parse(filename:join(WPSDir, "namelist.wps")),
    WRFTempl = nllist:parse(filename:join(WRFDir, "run/namelist.input")),
    Cfg1 = wrf_nl:read_config(WRFTempl),

    NLSpec = wrf_reg:create_profile_from_reg(filename:join(WRFDir, "Registry"),
					     vanilla_wrf_v34),

    Cfg = plist:update_with([ {wrf_install_dir, WRFDir},
			      {wps_install_dir, WPSDir},
			      {wrf_build_type, detect_wrf_build(WRFDir)},
			      {wrf_exec_method, immediate},
			      {job_name, "testjob0"},
			      {workspace_dir, cfg:get_conf(workspace_root)},
			      {wps_nl_template, WPSTempl},
			      {wrf_nl_template, WRFTempl},
			      {nl_spec, NLSpec},
			      {wrf_from, {{2013, 5, 1}, {0, 0, 0}}},
			      {wrf_to, {{2013, 5, 1}, {0, 30, 0}}},
			      {history_interval_min, 15},
			      {grib_interval_seconds, 1800},
			      {grib_sources, [rnrs_nam218]} ],
			      Cfg1),

    run_job(Cfg).



test_mpi_job() ->
    
    cfg:start(),
    inets:start(),
    stor:start(),
    
    {ok, WRFDir} = cfg:get_conf(default_wrf),
    {ok, WPSDir} = cfg:get_conf(default_wps),
    {ok, WorkspaceRoot} = cfg:get_conf(workspace_root),
    WPSTempl = nllist:parse(filename:join(WPSDir, "namelist.wps")),
    WRFTempl = nllist:parse(filename:join(WRFDir, "run/namelist.input")),
    Cfg1 = wrf_nl:read_config(WRFTempl),

    NLSpec = wrf_reg:create_profile_from_reg(filename:join(WRFDir, "Registry"),
					     vanilla_wrf_v34),

    From = {{2013, 5, 2}, {0, 0, 0}},
    To = {{2013, 5, 2}, {0, 30, 0}},

    Cfg = plist:update_with([ {wrf_install_dir, WRFDir},
			      {wps_install_dir, WPSDir},
			      {wrf_build_type, detect_wrf_build(WRFDir)},
			      {wrf_exec_method, immediate},
			      {job_name, "testjob-mpi-0"},
			      {workspace_dir, WorkspaceRoot},
			      {wps_nl_template, WPSTempl},
			      {wrf_nl_template, WRFTempl},
			      {nl_spec, NLSpec},
			      {wrf_from, From},
			      {wrf_to, To},
			      {history_interval_min, 15},
			      {grib_interval_seconds, atime:dt_time_diff(From, To)},
			      {grib_sources, [rnrs_nam218]},
			      {mpi_exec_name, "/usr/mpi/gcc/openmpi-1.4.3/bin/mpiexec"},
			      {mpi_nprocs, 12*4},
			      {mpi_nodes, [ "node01", "node02", "node03", "node04" ]} ],
			      Cfg1),

    run_job(Cfg).


run_job(Cfg) ->

    % FIXME: this is very basic, retrieve_grib_files needs to go into the retr module
    R = lists:nth(1, plist:getp(grib_sources, Cfg)),
    {{WPSFrom, WPSTo}, VtableFile, GribFiles} = retrieve_grib_files(R, plist:getp(wrf_from, Cfg), plist:getp(wrf_to, Cfg)),

    % construct temporary workspaces from job name
    JN = plist:getp(job_name, Cfg),
    WPSExecDir = filename:join(plist:getp(workspace_dir, Cfg), "wps_" ++ JN),
    WRFExecDir = filename:join(plist:getp(workspace_dir, Cfg), "wrf_" ++ JN),

    % ensure these workspaces are empty
    filesys:remove_directory(WPSExecDir),
    filesys:remove_directory(WRFExecDir),

    % given the limits in the GRIB files, update the configuration
    Cfg2 = plist:update_with([{grib_files, GribFiles},
			      {wps_from, WPSFrom},
			      {wps_to, WPSTo},
			      {vtable_file, VtableFile},
			      {wps_exec_dir, WPSExecDir},
			      {wrf_exec_dir, WRFExecDir}], Cfg),

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
    io:format("wrf_prep plan has ~p steps.~n", [plan:count_steps(Plan)]),

    % Find execution method to use
    ExecM = plist:getp(wrf_exec_method, Cfg),
    BuildT = plist:getp(wrf_build_type, Cfg),
    
    PID = plan_runner:execute_plan(Plan),
    case plan_runner:wait_for_plan(PID) of
	{success, _Log} ->
	    run_wrf(ExecM, BuildT, Cfg);
	{failure, _MFA, Text, _R, _Log} ->
	    io:format("error during plan execution [~p]~n", [lists:flatten(Text)]),
	    {failure, Text}
    end.


run_wrf(immediate, no_mpi, Cfg) ->
    Dir = plist:getp(wrf_exec_dir, Cfg),
    R = exec_tasks:execute(filename:join(Dir, "wrf.exe"),
			   [ {in_dir, Dir}, {output_type, stdout},
			     {exit_check, {scan_for, "SUCCESS"}},
			     {store_output_to, filename:join(Dir, "wrf.output")} ]),
    case R of
	{success, _Msg} ->
	    post_wrf(Cfg);
	_ ->
	    R
    end;

run_wrf(immediate, with_mpi, Cfg) ->

    % retrieve runtime parameters from configuration
    Dir = plist:getp(wrf_exec_dir, Cfg),
    Machines = plist:getp(mpi_nodes, Cfg),
    NP = integer_to_list(plist:getp(mpi_nprocs, Cfg)),

    % write a machinefile into the wrf directory
    file:write_file(filename:join(Dir, "node_list"), string:join(Machines, "\n")),

    % execute the mpiexec/mpirun command as per configuration
    R = exec_tasks:execute(plist:getp(mpi_exec_name, Cfg),
			   [{in_dir, Dir}, {output_type, filename:join(Dir, "rsl.error.0000")},
			    {exit_check, {scan_for, "SUCCESS"}},
			    {op_args, [{args, ["--machinefile", "node_list", "-n", NP, "./wrf.exe"]}]},
			    {store_output_to, filename:join(Dir, "wrf.output")}]),
    case R of
	{success, _Msg} ->
	    post_wrf(Cfg);
	_ ->
	    R
    end.



post_wrf(Cfg) ->
    JN = plist:getp(job_name, Cfg),
    WPSDir = plist:getp(wps_exec_dir, Cfg),
    WRFDir = plist:getp(wrf_exec_dir, Cfg),
    WPSL = [ "geogrid.output", "ungrib.output", "metgrid.output",
	     "geogrid.log", "ungrib.log", "metgrid.log",
	     "namelist.wps" ],
    WRFL = [ "namelist.input", "real.output", "wrf.output" ],

    Dom = "outputs/" ++ JN,
    stor:ensure_location(Dom, "test_file"),

    % store all fixed files
    lists:map(fun (X) -> stor:store(Dom, X, filename:join(WPSDir, X)) end, WPSL),
    lists:map(fun (X) -> stor:store(Dom, X, filename:join(WRFDir, X)) end, WRFL),

    % store wrfout files
    WRFOuts = filesys:list_dir_regexp(WRFDir, "wrfout.+"),
    lists:map(fun (X) -> stor:store(Dom, X, filename:join(WRFDir, X)) end, WRFOuts),

    % clean out and remove directories
    filesys:remove_directory(plist:getp(wps_exec_dir, Cfg)),
    filesys:remove_directory(plist:getp(wrf_exec_dir, Cfg)),
    success.


make_namelists(Cfg) ->
    WPSNL = wps_nl:write_config(Cfg, plist:getp(wps_nl_template, Cfg)),
    WRFNL = wrf_nl:write_config(Cfg, plist:getp(wrf_nl_template, Cfg)),
    plist:update_with([ {wps_nl, WPSNL}, {wrf_nl, WRFNL} ], Cfg).


retrieve_grib_files(R, From, To) ->
    Dom = apply(R, domain, []),
    URLBase = apply(R, url_prefix, []),
    {ok, Cov, IDs} = apply(R, manifest, [From, To, 0]),
    {Cov, apply(R, vtable, []), retrieve_grib_files(Dom, URLBase, IDs, [])}.


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

    FileList = [ "run/wrf.exe", "run/real.exe" ],
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
