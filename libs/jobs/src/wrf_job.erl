

-module(wrf_job).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-include_lib("util/include/job_desc.hrl").
-include_lib("flow/include/flow.hrl").
-export([check/1, execute/1]).


%% @doc
%% Checks whether the configuration structure is valid for wrf_job and makes
%% a best-effort check if this job will be executable.
%% @spec check_config(C::plist()) -> {success, []}|{failure, Reason}
%%
check(#job_desc{cfg=C}) ->
    WRFId = plist:getp(wrf_id, C),
    case tasks_verify:config_exists(WRFId) of
	{success, _} ->
	    WRFDir = wrfx_db:get_conf(WRFId),
	    check_config(wrf_inst:detect_build_type(WRFDir), C);
	R ->
	    R
    end.


check_config(no_mpi, C) ->

    Ts = [ {tasks_verify, check_keys,
	    [ [wrf_id, wps_id, wrf_exec_method, wps_nl_template_id,
	       wrf_nl_template_id, grib_sources, schedule], C ]},
	   {tasks_verify, namelist_exists, [plist:getp(wps_nl_template_id, C)]},
	   {tasks_verify, namelist_exists, [plist:getp(wrf_nl_template_id, C)]},
	   {tasks_verify, config_exists, [plist:getp(wps_id, C)]} ],

    PID = plan_runner:execute_plan(#plan{id=wrf_serial_job_check, tasks=Ts}, []),
    plan_runner:wait_for_plan(PID);

check_config(with_mpi, C) ->
    
    Ts = [ {tasks_verify, check_keys,
	    [ [wrf_id, wps_id, wrf_exec_method, wps_nl_template_id,
	       wrf_nl_template_id, grib_sources, schedule, mpi_exec_name,
	       mpi_nprocs, mpi_nodes], C ]},
	   {tasks_verify, namelist_exists, [plist:getp(wps_nl_template_id, C)]},
	   {tasks_verify, namelist_exists, [plist:getp(wrf_nl_template_id, C)]},
	   {tasks_verify, config_exists, [plist:getp(wps_id, C)]} ],

    PID = plan_runner:execute_plan(#plan{id=wrf_mpi_job_check, tasks=Ts}, []),
    plan_runner:wait_for_plan(PID).
    

execute(CfgOverw) ->

    % read the namelist configuration from Cfg1 and update what is necessary from Cfg2
    {success, WRFTempl} = wrfx_db:lookup({nllist, plist:getp(wrf_nl_template_id, CfgOverw)}),
    CfgNl = wrf_nl:read_config(WRFTempl),
    Cfg = plist:update_with(CfgOverw, CfgNl),

    % retrieve the scheduled time and construct the nominal run time
    T = case plist:getp(schedule, Cfg) of
	    now ->
		{_, {Hr, _M, _S}} = calendar:universal_time(),
		{Hr, 0, 0};
	    {Hr, _M, _S} ->
		{Hr, 0, 0}
	end,
    {Date, _} = calendar:universal_time(),
    DT = {Date, T},

    % retrieve start and end times which are specified relative to schedule or absolutely 
    case plist:contains(from_delta_hr, Cfg) of
	{true, _} ->
	    From = atime:dt_shift_hours(DT, plist:getp(from_delta_hr, Cfg)),
	    To = atime:dt_shift_hours(DT, plist:getp(to_delta_hr, Cfg));
	false ->
	    From = plist:getp(wrf_from, Cfg),
	    To = plist:getp(wrf_to, Cfg)
    end,

    % retrieve the installation directories
    WRFInstDir = wrfx_db:get_conf(plist:getp(wrf_id, Cfg)),
    WPSInstDir = wrfx_db:get_conf(plist:getp(wps_id, Cfg)),

    % build a namelist specification from the registry in wrf dir
    NLSpec = wrf_reg:create_profile_from_reg(filename:join(WRFInstDir, "Registry"),
					     vanilla_wrf_v34),

    % construct job name using From
    JN = io_lib:format("wrf_job_~s", [esmf:time_to_string(From)]),

    % retrieve GRIB files
    GribSrc = lists:nth(1, plist:getp(grib_sources, Cfg)),
    {{CovFrom, CovTo}, VtableFile, GribFiles} = retrieve_grib_files(GribSrc, From, To),

    % construct temporary workspaces from job name
    Wkspace = wrfx_db:get_conf(workspace_root),
    WPSExecDir = filename:join(Wkspace, "wps_exec_dir_for_" ++ JN),
    WRFExecDir = filename:join(Wkspace, "wrf_exec_dir_for_" ++ JN),

    % ensure these workspaces are empty
    filesys:remove_directory(WPSExecDir),
    filesys:remove_directory(WRFExecDir),

    % update cfg with wps: GRIB file time limits, vtable file to use and wrf: from-to range
    Cfg2 = plist:update_with([{grib_files, GribFiles},
			      {wrf_build_type, wrf_inst:detect_build_type(WRFInstDir)},
			      {wrf_from, From},
			      {wrf_to, To},
			      {wps_from, CovFrom},
			      {wps_to, CovTo},
			      {vtable_file, VtableFile},
			      {wps_exec_dir, WPSExecDir},
			      {wrf_exec_dir, WRFExecDir},
			      {wps_install_dir, WPSInstDir},
			      {wrf_install_dir, WRFInstDir},
			      {job_name, JN},
			      {nl_spec, NLSpec}], Cfg),

    % construct WRF and WPS namelists
    Cfg3 = make_namelists(Cfg2),

    % plan & execute WPS job
    Plan = plan_wps_exec:make_exec_plan(Cfg3),

    L = plan_logger:start(stdio),
    L2 = plan_logger:start("/tmp/wps_exec_plan.log"),
    PID = plan_runner:execute_plan(Plan, [L, L2]),
    case plan_runner:wait_for_plan(PID) of
     	{success, _} ->
     	    prep_wrf(Cfg3);
	R ->
	    R
    end.


prep_wrf(Cfg) ->

    % Make an execution plan and run it (this does everything except submitting the wrf_job
    Plan = plan_wrf_prep:make_exec_plan(Cfg),

    % Find execution method to use
    ExecM = plist:getp(wrf_exec_method, Cfg),
    BuildT = plist:getp(wrf_build_type, Cfg),
    
    L = plan_logger:start(stdio),
    PID = plan_runner:execute_plan(Plan, [L]),
    case plan_runner:wait_for_plan(PID) of
	{success,_} ->
	    run_wrf(ExecM, BuildT, Cfg);
	R ->
	    R
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
    MPI = plist:getp(mpi_exec_name, Cfg),

    % write a machinefile into the wrf directory
    file:write_file(filename:join(Dir, "node_list"), string:join(Machines, "\n")),

    % execute the mpiexec/mpirun command as per configuration
    R = tasks_exec:execute(MPI,
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
    lists:map(fun (X) -> wrfx_fstor:store({Dom, X}, filename:join(WPSDir, X)) end, WPSL),
    lists:map(fun (X) -> wrfx_fstor:store({Dom, X}, filename:join(WRFDir, X)) end, WRFL),

    % store wrfout files
    WRFOuts = filesys:list_dir_regexp(WRFDir, "wrfout.+"),
    lists:map(fun (X) -> wrfx_fstor:store({Dom, X}, filename:join(WRFDir, X)) end, WRFOuts),

    % clean out and remove directories
    filesys:remove_directory(plist:getp(wps_exec_dir, Cfg)),
    filesys:remove_directory(plist:getp(wrf_exec_dir, Cfg)),

    success.


make_namelists(Cfg) ->
    {success, WPST} = wrfx_db:lookup({nllist, plist:getp(wps_nl_template_id, Cfg)}),
    {success, WRFT} = wrfx_db:lookup({nllist, plist:getp(wrf_nl_template_id, Cfg)}),
    io:format("~p~n", [nllist:namelists(WPST)]),
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
    case wrfx_fstor:exists({Dom, Name}) of
	false ->
	    io:format("Downloading [~p]~n", [URLBase ++ Name]),
	    {success, _} = tasks_net:http_sync_get(URLBase ++ Name, "/tmp/wrfx-download"),
	    {success, F} = wrfx_fstor:store({Dom, Name}, "/tmp/wrfx-download"),
	    retrieve_grib_files(Dom, URLBase, Names, [F|List]);
	{true, F} ->
	    retrieve_grib_files(Dom, URLBase, Names, [F|List])
    end.
