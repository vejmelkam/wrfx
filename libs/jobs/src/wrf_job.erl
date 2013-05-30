

-module(wrf_job).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-include_lib("flow/include/flow.hrl").
-export([check_config/1, run_job/1, test_serial_job/0, test_mpi_job/0]).


%% @doc
%% Checks whether the configuration structure is valid for wrf_job and makes
%% a best-effort check if this job will be executable.
%% @spec check_config(C::plist()) -> true|{false, Error}
%%
check_config(C) ->

    case plist:contains_keys([wrf_id, wps_id, wrf_exec_method,
			      wps_nl_template_id, wrf_nl_template_id,
			      grib_sources, schedule])
    of
	{false, Missing} ->
	    {false, {missing_keys, Missing}};
	true ->
	    WrfId = plist:getp(wrf_id, C),
	    {ok, WRFDir} = wrfx_cfg:get_conf(WrfId),
	    BT = wrf_inst:detect_build_type(WRFDir),
	    check_run_config(BT, C)
    end.

check_run_config(no_mpi, _C) ->
    true;
check_run_config(with_mpi, C) ->
    case plist:contains_keys([mpi_exec_name, mpi_nprocs, mpi_nodes], C) of
	true ->
	    true;
	{false, M} ->
	    {false, {missing_keys, M}}
    end.


%% @doc
%% It's still not clear how to generate and store complex plans (jobs).
%% For now a job plan is generated at run time using a class such as this.
%% The job is configured using a plist, which must contain (for this job):
%
%    - wps_nl_template: a WPS namelist (as parsed by nllist:parse/1)
%    - wrf_nl_template: a WRF namelist (as parsed by nllist:parse/1)
%    - wrf_root_dir: the directory of the WRF installation to be used
%    - grib_files: list of GRIB files for use by ungrib.exe
%    - vtable_file: WPS-directory relative name of the Vtable file to use with ungrib.exe
%

test_serial_job() ->

    wrfx:start(),
    
%    {ok, WRFDir} = wrfx_cfg:get_conf(serial_wrf_34),
%    {ok, WPSDir} = wrfx_cfg:get_conf(default_wps),

    Cfg = [ {wrf_id, serial_wrf_34},
	    {wps_id, default_wps},
	    {wrf_build_type, no_mpi},
	    {wrf_exec_method, immediate},
	    {wps_nl_template_id, colorado_test_wps},
	    {wrf_nl_template_id, colorado_test_wrf},
	    {wrf_from, {{2013, 5, 1}, {0, 0, 0}}},
	    {wrf_to, {{2013, 5, 1}, {0, 30, 0}}},
	    {history_interval_min, 15},
	    {grib_sources, [rnrs_nam218]},
	    {schedule, {23, 0, 0}}],

    run_job(Cfg).



test_mpi_job() ->
    
%    NLSpec = wrf_reg:create_profile_from_reg(filename:join(WRFDir, "Registry"),
%					     vanilla_wrf_v34),

    From = {{2013, 5, 2}, {0, 0, 0}},
    To = {{2013, 5, 2}, {0, 30, 0}},

    Cfg = [ {wrf_id, serial_wrf_34},
	    {wps_id, default_wps},
	    {wrf_build_type, with_mpi},
	    {wrf_exec_method, immediate},
	    {wps_nl_template_id, colorado_test_wps},
	    {wrf_nl_template_id, colorado_test_wrf},
	    {wrf_from, From},
	    {wrf_to, To},
	    {history_interval_min, 15},
	    {grib_sources, [rnrs_nam218]},
	    {mpi_exec_name, "/usr/mpi/gcc/openmpi-1.4.3/bin/mpiexec"},
	    {mpi_nprocs, 12*4},
	    {mpi_nodes, [ "node01", "node02", "node03", "node04" ]} ],

    run_job(Cfg).


run_job(CfgOverw) ->

    % read the namelist configuration from Cfg1 and update what is necessary from Cfg2
    {ok, WRFTempl} = wrfx_stor:namelist_retrieve(plist:getp(wrf_nl_template_id, CfgOverw)),
    CfgNl = wrf_nl:read_config(WRFTempl),
    Cfg = plist:update_with(CfgOverw, CfgNl),

    % retrieve the scheduled time and construct the nominal run time
    T = plist:getp(schedule, Cfg),
    {Date, _} = calendar:universal_time(),
    DT = {Date, T},

    % retrieve start and end times which are specified relative to schedule or absolutely 
    case plist:contains(start_delta_hr, Cfg) of
	{true, _} ->
	    From = atime:dt_shift_hours(DT, plist:getp(start_delta_hr, Cfg)),
	    To = atime:dt_shift_hours(DT, plist:getp(stop_delta_hr, Cfg));
	false ->
	    From = plist:getp(wrf_from, Cfg),
	    To = plist:getp(wrf_to, Cfg)
    end,

    % retrieve the installation directories
    {ok, WRFInstDir} = wrfx_cfg:get_conf(plist:getp(wrf_id, Cfg)),
    {ok, WPSInstDir} = wrfx_cfg:get_conf(plist:getp(wps_id, Cfg)),

    % build a namelist specification from the registry in wrf dir
    NLSpec = wrf_reg:create_profile_from_reg(filename:join(WRFInstDir, "Registry"),
					     vanilla_wrf_v34),

    % construct job name using From
    JN = io_lib:format("wrf_job_~s", [esmf:time_to_string(From)]),

    % retrieve GRIB files
    R = lists:nth(1, plist:getp(grib_sources, Cfg)),
    {{WPSFrom, WPSTo}, VtableFile, GribFiles} = retrieve_grib_files(R, From, To),

    % construct temporary workspaces from job name
    {ok, Wkspace} = wrfx_cfg:get_conf(workspace_root),
    WPSExecDir = filename:join(Wkspace, "wps_" ++ JN),
    WRFExecDir = filename:join(Wkspace, "wrf_" ++ JN),

    % ensure these workspaces are empty
    filesys:remove_directory(WPSExecDir),
    filesys:remove_directory(WRFExecDir),

    % update cfg with wps: GRIB file time limits, vtable file to use and wrf: from-to range
    Cfg2 = plist:update_with([{grib_files, GribFiles},
			      {wrf_from, From},
			      {wrf_to, To},
			      {wps_from, WPSFrom},
			      {wps_to, WPSTo},
			      {vtable_file, VtableFile},
			      {wps_exec_dir, WPSExecDir},
			      {wrf_exec_dir, WRFExecDir},
			      {wps_install_dir, WPSInstDir},
			      {wrf_install_dir, WRFInstDir},
			      {job_name, JN},
			      {grib_interval_seconds, atime:dt_time_diff(From, To)},
			      {nl_spec, NLSpec}], Cfg),

    % construct WRF and WPS namelists
    Cfg3 = make_namelists(Cfg2),

    % plan & execute WPS job
    Plan = plan_wps_exec:make_exec_plan(Cfg3),

    L = plan_logger:start(stdio),
    L2 = plan_logger:start("/tmp/wps_exec_plan.log"),
    PID = plan_runner:execute_plan(Plan, [L, L2]),
    case plan_runner:wait_for_plan(PID) of
     	success ->
     	    prep_wrf(Cfg3);
	failure ->
	    failure
    end.


prep_wrf(Cfg) ->

    % Make an execution plan and run it (this does everything except submitting the wrf_job
    Plan = plan_wrf_prep:make_exec_plan(Cfg),
    io:format("wrf_prep plan has ~p steps.~n", [plan:count_steps(Plan)]),

    % Find execution method to use
    ExecM = plist:getp(wrf_exec_method, Cfg),
    BuildT = plist:getp(wrf_build_type, Cfg),
    
    L = plan_logger:start(stdio),
    PID = plan_runner:execute_plan(Plan, [L]),
    case plan_runner:wait_for_plan(PID) of
	success ->
	    run_wrf(ExecM, BuildT, Cfg);
	failure ->
	    failure
    end.


run_wrf(immediate, no_mpi, Cfg) ->
    Dir = plist:getp(wrf_exec_dir, Cfg),
    R = tasks_exec:execute(filename:join(Dir, "wrf.exe"),
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
    R = tasks_exec:execute(plist:getp(mpi_exec_name, Cfg),
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


%% @TODO: this step should be converted into a plan as well
post_wrf(Cfg) ->
    WPSDir = plist:getp(wps_exec_dir, Cfg),
    WRFDir = plist:getp(wrf_exec_dir, Cfg),
    WPSL = [ "geogrid.output", "ungrib.output", "metgrid.output",
	     "geogrid.log", "ungrib.log", "metgrid.log",
	     "namelist.wps" ],
    WRFL = [ "namelist.input", "real.output", "wrf.output" ],

    Dom = "outputs/" ++ plist:getp(job_name, Cfg),

    % store all fixed files
    lists:map(fun (X) -> wrfx_stor:file_store({Dom, X}, filename:join(WPSDir, X)) end, WPSL),
    lists:map(fun (X) -> wrfx_stor:file_store({Dom, X}, filename:join(WRFDir, X)) end, WRFL),

    % store wrfout files
    WRFOuts = filesys:list_dir_regexp(WRFDir, "wrfout.+"),
    lists:map(fun (X) -> wrfx_stor:file_store({Dom, X}, filename:join(WRFDir, X)) end, WRFOuts),

    % clean out and remove directories
    filesys:remove_directory(plist:getp(wps_exec_dir, Cfg)),
    filesys:remove_directory(plist:getp(wrf_exec_dir, Cfg)),

    success.


make_namelists(Cfg) ->
    {ok, WPST} = wrfx_stor:namelist_retrieve(plist:getp(wps_nl_template_id, Cfg)),
    {ok, WRFT} = wrfx_stor:namelist_retrieve(plist:getp(wrf_nl_template_id, Cfg)),
    WPSNL = wps_nl:write_config(Cfg, WPST),
    WRFNL = wrf_nl:write_config(Cfg, WRFT),
    plist:update_with([ {wps_nl, WPSNL}, {wrf_nl, WRFNL} ], Cfg).


retrieve_grib_files(R, From, To) ->
    Dom = apply(R, domain, []),
    URLBase = apply(R, url_prefix, []),
    {ok, Cov, IDs} = apply(R, manifest, [From, To, 0]),
    {Cov, apply(R, vtable, []), retrieve_grib_files(Dom, URLBase, IDs, [])}.


retrieve_grib_files(_Dom, _URL, [], List) ->
    List;

retrieve_grib_files(Dom, URLBase, [Name|Names], List) ->
    case wrfx_stor:file_exists({Dom, Name}) of
	false ->
	    io:format("Downloading [~p]~n", [URLBase ++ Name]),
	    {success, _} = tasks_net:http_sync_get(URLBase ++ Name, "/tmp/wrfx-download"),
	    {success, F} = wrfx_stor:file_store({Dom, Name}, "/tmp/wrfx-download"),
	    retrieve_grib_files(Dom, URLBase, Names, [F|List]);
	{true, F} ->
	    retrieve_grib_files(Dom, URLBase, Names, [F|List])
    end.

    

