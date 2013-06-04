

-module(wrf_job).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-include_lib("jobs/include/jobs.hrl").
-include_lib("flow/include/flow.hrl").
-export([check/1, execute/1]).


%% @doc
%% Checks whether the configuration structure is valid for wrf_job and makes
%% a best-effort check if this job will be executable.
%% @spec check_config(C::plist()) -> {success, []}|{failure, Reason}
%%
check(#job_desc{cfg=C}) ->
    WRFId = plist:getp(wrf_id, C),
    Ts = [ {tasks_verify, config_exists, [WRFId]},
	   {tasks_verify, conditional_check, [ncks_prune_wrfout, { tasks_verify, config_exists, [ncks_path] }, C]} ],
    PID = plan_runner:execute_plan(#plan{id=wrf_job_check, tasks=Ts}, []),
    case plan_runner:wait_for_plan(PID) of
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
    

execute(J=#job_desc{key = JK, cfg=CfgOverw}) ->

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
    JI = io_lib:format("~s_~s", [JK, esmf:time_to_string(From)]),

    % construct temporary workspaces from job name
    Wkspace = wrfx_db:get_conf(workspace_root),
    WPSExecDir = filename:join(Wkspace, "wps_exec_" ++ JI),
    WRFExecDir = filename:join(Wkspace, "wrf_exec_" ++ JI),

    % ensure these workspaces are empty
    filesys:remove_directory(WPSExecDir),
    filesys:remove_directory(WRFExecDir),

    % retrieve GRIB files
    GribSrc = lists:nth(1, plist:getp(grib_sources, Cfg)),
    {{CovFrom, CovTo}, VtableFile, NLExtraKeys, GribFiles} = retrieve_grib_files(GribSrc, From, To),

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
			      {job_id, JI},
			      {nl_spec, NLSpec},
			      {started, calendar:local_time()},
			      {instr, []} | NLExtraKeys ], Cfg),

    % construct WRF and WPS namelists
    Cfg3 = make_namelists(Cfg2),

    % open a logger that will record all plan activity
    Log = logd:open([stdio, filename:join(Wkspace, io_lib:format("~s.log", [JI]))]),

    run_wps(J#job_desc{cfg=Cfg3}, Log).


run_wps(J=#job_desc{cfg=Cfg}, Log) ->
    WPSDir = plist:getp(wps_install_dir, Cfg),             % directory with an installation of WPS
    ExecDir = plist:getp(wps_exec_dir, Cfg),               % directory in which WPS step is supposed to run
    Vtable = plist:getp(vtable_file, Cfg),                 % Vtable file relative to WPS directory
    WPSNL = plist:getp(wps_nl, Cfg),                       % namelist for wps
    GRIBFiles = plist:getp(grib_files, Cfg),               % list of GRIB files

    % files that must be symlinked from the workspace directory
    Files = ["geogrid.exe", "geogrid", "metgrid.exe", "metgrid", "ungrib.exe", "ungrib"],

    T = [ % create target directory
	  {tasks_fsys, create_dir, [ExecDir]},

	  % symlink files from install to execution directory
	  [ { tasks_fsys, create_symlink,
	      [filename:join(WPSDir, F), filename:join(ExecDir, F)] } || F <- Files ],

	  % symlink vtable (depends on GRIB file source)
	  { tasks_fsys, create_symlink,
	    [filename:join(WPSDir, Vtable), filename:join(ExecDir, "Vtable")] },

	  % write the constructed WPS namelist into namelist.wps
	  {tasks_fsys, write_file,
	   [filename:join(ExecDir, "namelist.wps"), nllist:to_text(WPSNL)]},

	  % run geogrid.exe, store output in geogrid.output
	  #instr_task{
	     mfa = {tasks_exec, execute,
		    [filename:join(ExecDir, "geogrid.exe"), 
		     [{output_type, stdout},  {in_dir, ExecDir},
		      {exit_check, {scan_for, "Successful completion of geogrid"}},
		      {store_output_to, filename:join(ExecDir, "geogrid.output")}]]},
	     with_key = geogrid,
	     what = [run_time]},

	  % link in all grib files with correct names GRIBFILE.AAA
	  [ { tasks_fsys, create_symlink,
	      [X, filename:join(ExecDir, Y)]} || {X,Y} <- make_grib_names(GRIBFiles) ],

	  % execute ungrib.exe, store output in ungrib.output
	  #instr_task{
	     mfa = {tasks_exec, execute,
		    [filename:join(ExecDir, "ungrib.exe"),
		     [{output_type, stdout},  {in_dir, ExecDir},
		      {exit_check, {scan_for, "Successful completion of ungrib"}},
		      {store_output_to, filename:join(ExecDir, "ungrib.output")}]]},
	     with_key = ungrib,
	     what = [run_time]},

	  % execute metgrid.exe, store output in metgrid.output
	  #instr_task{
	     mfa = {tasks_exec, execute,
		    [filename:join(ExecDir, "metgrid.exe"),
		     [{output_type, stdout},  {in_dir, ExecDir},
		      {exit_check, {scan_for, "Successful completion of metgrid"}},
		      {store_output_to, filename:join(ExecDir, "metgrid.output")}]]},
	     with_key = metgrid,
	     what = [run_time]}

	],
	  
    Plan = #plan{id=plan_wps_exec, tasks=lists:flatten(T)},
    
    PL = plan_logger:start(Log),
    PID = plan_runner:execute_plan(Plan, [PL]),
    case plan_runner:wait_for_plan(PID) of
     	{success, PlanInstr} ->
	    NewInstr = plist:update_with(PlanInstr, plist:getp(instr, Cfg)),
     	    prep_wrf(J#job_desc{cfg=plist:setp(instr, NewInstr, Cfg)}, Log);
	R ->
	    job_failed(run_wps, J, Log, R)
    end.
    


prep_wrf(J=#job_desc{cfg=Cfg}, Log) ->
    WPSExecDir = plist:getp(wps_exec_dir, Cfg),
    WRFDir = filename:join(plist:getp(wrf_install_dir, Cfg), "run"),     % directory, which is setup to run WRF
    ExecDir = plist:getp(wrf_exec_dir, Cfg),                             % directory in which WPS step is supposed to run
    WRFNL = plist:getp(wrf_nl, Cfg),                                     % namelist for wps
    BuildType = plist:getp(wrf_build_type, Cfg),                         % MPI compiled or not?

    % files that must be symlinked from the workspace directory
    Files = [ "CAM_ABS_DATA", "CAM_AEROPT_DATA", "co2_trans", "ETAMPNEW_DATA", "ETAMPNEW_DATA_DBL",
	      "ETAMPNEW_DATA.expanded_rain", "ETAMPNEW_DATA.expanded_rain_DBL", "GENPARM.TBL",
	      "gribmap.txt", "grib2map.tbl", "LANDUSE.TBL", "MPTABLE.TBL", "namelist.fire",
	      "ozone.formatted", "ozone_lat.formatted", "ozone_plev.formatted",
	      "real.exe", "RRTM_DATA", "RRTM_DATA_DBL", "RRTMG_LW_DATA", "RRTMG_LW_DATA_DBL",
	      "RRTMG_SW_DATA", "RRTMG_SW_DATA_DBL", "SOILPARM.TBL", "tc.exe", "tr49t67", "tr49t85",
	      "tr67t85", "URBPARM.TBL", "URBPARM_UZE.TBL", "VEGPARM.TBL", "wrf.exe" ],

    MET_Files = filesys:list_dir_regexp(WPSExecDir, "met_em.+"),

    T = [ {tasks_fsys, create_dir, [ExecDir]},
	  [ { tasks_fsys, create_symlink, [filename:join(WRFDir, F), filename:join(ExecDir, F)] } || F <- Files ],
	  [ { tasks_fsys, create_symlink, [filename:join(WPSExecDir, F), filename:join(ExecDir, F)] } || F <- MET_Files ],
	  {tasks_fsys, write_file, [filename:join(ExecDir, "namelist.input"), nllist:to_text(WRFNL)]},
	  #instr_task{
	     mfa = {tasks_exec, execute, [filename:join(ExecDir, "real.exe"), 
					  [{in_dir, ExecDir}, {output_type, real_exe_output(BuildType, ExecDir)},
					   {exit_check, {scan_for, "SUCCESS COMPLETE REAL_EM"}},
					   {store_output_to, filename:join(ExecDir, "real.output")}]]},
	     with_key = "real",
	     what = [run_time]}
	],
	  
    Plan = #plan{id=wrf_prep_exec, tasks=lists:flatten(T)},

    % Find execution method to use
    ExecM = plist:getp(wrf_exec_method, Cfg),
    BuildT = plist:getp(wrf_build_type, Cfg),
    
    PL = plan_logger:start(Log),
    PID = plan_runner:execute_plan(Plan, [PL]),
    case plan_runner:wait_for_plan(PID) of
	{success,_} ->
	    run_wrf(ExecM, BuildT, J, Log);
	R ->
	    job_failed(prepare_wrf, J, Log, R)
    end.


run_wrf(immediate, no_mpi, J=#job_desc{cfg=Cfg}, Log) ->
    Dir = plist:getp(wrf_exec_dir, Cfg),
    R = tasks_exec:execute(filename:join(Dir, "wrf.exe"),
			   [ {in_dir, Dir}, {output_type, stdout},
			     {exit_check, {scan_for, "SUCCESS"}},
			     {store_output_to, filename:join(Dir, "wrf.output")} ]),
    case R of
	{success, _Msg} ->
	    post_wrf(J, Log);
	_ ->
	    job_failed(run_wrf, J, Log, R)
    end;

run_wrf(immediate, with_mpi, J=#job_desc{cfg=Cfg}, Log) ->

    logd:message("setting up immediate parallel run of WRF", Log),
    
    % retrieve runtime parameters from configuration
    Dir = plist:getp(wrf_exec_dir, Cfg),
    Machines = plist:getp(mpi_nodes, Cfg),
    NP = integer_to_list(plist:getp(mpi_nprocs, Cfg)),
    MPI = plist:getp(mpi_exec_name, Cfg),

    logd:message(io_lib:fwrite("creating machine file with nodes ~p", [Machines]), Log),

    % write a machinefile into the wrf directory
    file:write_file(filename:join(Dir, "node_list"), string:join(Machines, "\n")),

    logd:message("invoking mpiexec", Log),

    Start = calendar:local_time(),

    % execute the mpiexec/mpirun command as per configuration
    R = tasks_exec:execute(MPI,
			   [{in_dir, Dir},
			    {output_type, filename:join(Dir, "rsl.error.0000")},
			    {exit_check, {scan_for, "SUCCESS"}},
			    {op_args, [{args, ["--machinefile", "node_list", "-n", NP, "./wrf.exe"]}]},
			    {store_output_to, filename:join(Dir, "wrf.output")}]),
    case R of
	{success, _Msg} ->
	    logd:message(io_lib:fwrite("mpiexec/WRF execution success after ~p seconds.", [atime:dt_seconds_between(Start, calendar:local_time())]), Log),
	    prune_wrfouts(plist:getp(ncks_prune_wrfout, Cfg, no_pruning), J, Log);
	_ ->
	    job_failed(run_wrf, J, Log, R)
    end.


prune_wrfouts(no_pruning, J, Log) ->
    logd:message("no pruning requested, skipping stage"),
    post_wrf(J, Log);
prune_wrfouts(Vars, J = #job_desc{cfg=Cfg}, Log) ->
    Dir = plist:getp(wrf_exec_dir, Cfg),
    NCKS = wrfx_db:get_conf(ncks_path),
    AbsFs = lists:map(fun (X) -> filename:join(Dir, X) end, filesys:list_dir_regexp(Dir, "wrfout.+")),
    T = [ [ {tasks_exec, execute, [ NCKS, [ {in_dir, Dir},
					    {output_type, stdout},
					    {op_args, [{args, ["-v", string:join(Vars, ","), File, File ++ "_pruned"]}]},
					    {store_output_to, filename:join(Dir, "ncks.log")},
					    {exit_check, exit_code} ] ]},
	    {tasks_fsys, delete_file, [File]},
	    {tasks_fsys, rename_file, [File ++ "_pruned", File]} ] || File <- AbsFs ],
    Plan = #plan{id=prune_wrfout, tasks=lists:flatten(T)},
    PL = plan_logger:start(Log),
    PID = plan_runner:execute_plan(Plan, [PL]),
    plan_runner:wait_for_plan(PID),
    % post_wrfouts is not a critical step in the plan, so no job failure occurrs if the wrfouts cannot be pruned
    post_wrf(J, Log).
    


post_wrf(J=#job_desc{key=JK, cfg=Cfg}, Log) ->
    store_output_files(J, Log),
    JI = plist:getp(job_id, Cfg),
    Dom = "outputs/" ++ JI,
    LFName = lists:flatten(io_lib:format("~s.log", [JI])),

    % remove workspace directories
    filesys:remove_directory(plist:getp(wps_exec_dir, Cfg)),
    filesys:remove_directory(plist:getp(wrf_exec_dir, Cfg)),

    % return a job report
    #job_report{job_id = plist:getp(job_id, Cfg),
		job_desc_key = JK,
		started = plist:getp(started, Cfg),
		completed = calendar:local_time(),
		result = {success, "SUCCESS"},
		wksp_dir = plist:subset([wrf_exec_dir, wps_exec_dir], Cfg),
		stor_dom = Dom,
		log_stor_id = {Dom, LFName},
		inst = plist:getp(instr, Cfg)}.
		



job_failed(A, J=#job_desc{key=JK, cfg=Cfg}, Log, {failure, Err}) ->

    ErrMsg = lists:flatten(io_lib:format("job execution failed in step ~p error ~s~n", [A, Err])),
    logd:message(ErrMsg, Log),

    store_output_files(J, Log),
    JI = plist:getp(job_id, Cfg),
    Dom = "outputs/" ++ JI,
    LFName = lists:flatten(io_lib:format("~s.log", [JI])),

    % return a job report
    #job_report{job_id = plist:getp(job_id, Cfg),
		job_desc_key = JK,
		started = plist:getp(started, Cfg),
		completed = calendar:local_time(),
		result = {failure, ErrMsg},
		wksp_dir = plist:subset([wrf_exec_dir, wps_exec_dir], Cfg),
		stor_dom = Dom,
		log_stor_id = {Dom, LFName},
		inst = plist:getp(instr, Cfg)}.


store_output_files(#job_desc{cfg=Cfg}, Log) ->
    WPSDir = plist:getp(wps_exec_dir, Cfg),
    WRFDir = plist:getp(wrf_exec_dir, Cfg),
    JI = plist:getp(job_id, Cfg),

    logd:message("moving output files to storage", Log),
    Dom = "outputs/" ++ plist:getp(job_id, Cfg),

    T1 = [ {wrfx_fstor, store, [{Dom, X}, filename:join(WPSDir, X)]} ||
	     X <- [ "geogrid.output", "ungrib.output", "metgrid.output", "geogrid.log",
		    "ungrib.log", "metgrid.log", "namelist.wps" ] ],
    T2 = [ {wrfx_fstor, store, [{Dom, X}, filename:join(WRFDir, X)]} ||
	     X <- [ "namelist.input", "real.output", "wrf.output" ] ],
    
    T3 = [ {wrfx_fstor, store, [{Dom, X}, filename:join(WRFDir, X)]} ||
	     X <- filesys:list_dir_regexp(WRFDir, "wrfout.+") ],

    Plan = #plan{id = post_wrf_plan, tasks = lists:append([T1, T2, T3])},
    
    PL = plan_logger:start(Log),
    PID = plan_runner:execute_plan(Plan, [PL]),
    case plan_runner:wait_for_plan(PID) of
	{success, _T} ->
	    logd:message(io_lib:format("output files moved to ~s", [Dom]), Log);
	{failure, E} ->
	    logd:message(io_lib:format("failed to move all files with error ~s", [E]), Log)
    end,

    % close log and move it to storage
    logd:close(Log),
    LFName = io_lib:format("~s.log", [JI]),
    PathLF = filename:join(wrfx_db:get_conf(workspace_root), LFName),
    wrfx_fstor:store({Dom, LFName}, PathLF).



make_namelists(Cfg) ->
    {success, WPST} = wrfx_db:lookup({nllist, plist:getp(wps_nl_template_id, Cfg)}),
    {success, WRFT} = wrfx_db:lookup({nllist, plist:getp(wrf_nl_template_id, Cfg)}),
    WPSNL = wps_nl:write_config(Cfg, WPST),
    WRFNL = wrf_nl:write_config(Cfg, WRFT),
    plist:update_with([ {wps_nl, WPSNL}, {wrf_nl, WRFNL} ], Cfg).


retrieve_grib_files(R, From, To) ->
    Dom = R:domain(),
    URLBase = R:url_prefix(),
    {ok, Cov, IDs} = R:manifest(From, To, 0),
    {Cov, R:vtable(), R:nl_updates(), retrieve_grib_files(Dom, URLBase, IDs, [])}.


retrieve_grib_files(_Dom, _URL, [], List) ->
    List;

retrieve_grib_files(Dom, URLBase, [Name|Names], List) ->
    case wrfx_fstor:exists({Dom, Name}) of
	false ->
	    {success, _} = tasks_net:http_sync_get(URLBase ++ Name, "/tmp/wrfx-download"),
	    {success, F} = wrfx_fstor:store({Dom, Name}, "/tmp/wrfx-download"),
	    retrieve_grib_files(Dom, URLBase, Names, [F|List]);
	{true, F} ->
	    retrieve_grib_files(Dom, URLBase, Names, [F|List])
    end.



real_exe_output(no_mpi, _Dir) ->
    stdout;
real_exe_output(with_mpi, Dir) ->
    filename:join(Dir, "rsl.error.0000").

make_grib_names(GF) ->
    make_names(GF, $A, $A, $A, []).

make_names([], _, _, _, P) ->
    P;
%note $Z+1 = 91, erlang parser does not like pattern $Z+1
make_names(_G, 91, _E2, _E3, _P) ->
    too_many_grib_files;
make_names(G, E1, 91, E3, P) ->
    make_names(G, E1+1, $A, E3, P);
make_names(G, E1, E2, 91, P) ->
    make_names(G, E1, E2+1, $A, P);
make_names([G|GF], E1, E2, E3, P) ->
    make_names(GF, E1, E2, E3+1, [{G, "GRIBFILE." ++ [E1, E2, E3]}|P]).


