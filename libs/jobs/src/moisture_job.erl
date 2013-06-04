

%% @doc
%% This moisture job runs on a given wrfout file.
%% It first generates a list of xls files that are necessary to run the job.
%% These are downloaded as needed from the MesoWest server.
%% Then obs files are created according to the table.
%%

-module(moisture_job).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-include_lib("jobs/include/jobs.hrl").
-include_lib("flow/include/flow.hrl").
-export([check/1, execute/1]).

-define(DOMAIN, "mwest").
-define(MWEST_DL_URL, "http://mesowest.utah.edu/cgi-bin/droman/meso_download_mesowest_ndb.cgi").


%% @doc
%% Checks whether the configuration structure is valid for wrf_job and makes
%% a best-effort check if this job will be executable.
%% @spec check_config(C::plist()) -> {success, []}|{failure, Reason}
%%
check(_J=#job_desc{cfg=_C}) ->
    {success, "configuration check success"}.


execute(J=#job_desc{cfg=Cfg}) ->
    [From, To, Ss] = plist:get_list([wrf_from, wrf_to, stations], Cfg),

    JI = "moisture_job_" ++ format_time(element(1, From)),

    % construct temporary workspaces from job name
    Wkspace = wrfx_db:get_conf(workspace_root),
    Dir = filename:join(Wkspace, "moisture_exec_" ++ JI),

    Cfg2 = plist:update_with([{job_id, JI},
			      {job_domain, "outputs/" ++ JI},
			      {exec_dir, Dir},
			      {started, calendar:local_time()},
			      {instr, []}], Cfg),

    Log = logd:open([stdio, filename:join(Dir, "moisture_job.log")]),

    % retrieve all xls files from Mesowest (or from file cache)
    Fs = retrieve_xls_files(atime:dt_covering_days(From, To), Ss),

    Ts = [ {tasks_fsys, create_symlink, [F, filename:join(Dir, filename:basename(F))]}
	   || F <- Fs ],
    Plan = #plan{id=moisture_link_xls, tasks=Ts},
    PL = plan_logger:start(Log),
    PID = plan_runner:execute_plan(Plan, [PL]),
    case plan_runner:wait_for_plan(PID) of
	{success, _} ->
	    convert_to_obs(J#job_desc{cfg=Cfg2}, Log);
	R ->
	    job_failed(get_mesowest_files, J#job_desc{cfg=Cfg2}, Log, R)
    end.


convert_to_obs(J=#job_desc{cfg=Cfg}, Log) ->
    Table = plist:getp(obs_var_talbe, Cfg),
    Dir = plist:getp(exec_dir, Cfg),
    Ss = plist:getp(stations, Cfg),

    Ts = [ {tasks_fsys, write_file, [filename:join(Dir, "obs_var_table"), Table]},
	   {tasks_fsys, write_file, [filename:join(Dir, "station_list"), string:join(Ss, "\n") ++ "\n"]},
	   {tasks_exec, execute, ["deps/scraper/extract_observations.py",
				  [ {in_dir, Dir},
				    {output_type, stdout},
				    {op_args, [{args, ["station_list", "obs_var_table"]}]},
				    {store_output_to, filename:join(Dir, "extract_observations.log")},
				    {exit_check, exit_code}]]}],
    Plan = #plan{id=xls_to_obs, tasks=Ts},
    PL = plan_logger:start(Log),
    PID = plan_runner:execute_plan(Plan, [PL]),
    case plan_runner:wait_for_plan(PID) of
	{success, _} ->
	    run_moisture_code(J, Log);
	R ->
	    job_failed(xls_to_obs, J, Log, R)
    end.



run_moisture_code(J=#job_desc{key=JK, cfg=Cfg}, Log) ->
    store_output_files(J, Log),
    logd:close(Log),

    JI = plist:getp(job_id, Cfg),
    Dom = plist:getp(job_domain, Cfg),
    LFName = lists:flatten(io_lib:format("~s.log", [JI])),

    % return a job report
    #job_report{job_id = plist:getp(job_id, Cfg),
		job_desc_key = JK,
		started = plist:getp(started, Cfg),
		completed = calendar:local_time(),
		result = {success, "SUCCESS"},
		wksp_dir = plist:subset([exec_dir], Cfg),
		stor_dom = Dom,
		log_stor_id = {Dom, LFName},
		inst = plist:getp(instr, Cfg)}.



job_failed(A, J=#job_desc{key=JK, cfg=Cfg}, Log, {failure, Err}) ->

    ErrMsg = lists:flatten(io_lib:format("moisture job failed in step ~p error ~s~n", [A, Err])),
    logd:message(ErrMsg, Log),

    store_output_files(J, Log),

    JI = plist:getp(job_id, Cfg),
    Dom = plist:getp(job_domain, Cfg),
    LFName = lists:flatten(io_lib:format("~s.log", [JI])),

    % return a job report
    #job_report{job_id = plist:getp(job_id, Cfg),
		job_desc_key = JK,
		started = plist:getp(started, Cfg),
		completed = calendar:local_time(),
		result = {failure, ErrMsg},
		wksp_dir = plist:subset([exec_dir], Cfg),
		stor_dom = Dom,
		log_stor_id = {Dom, LFName},
		inst = plist:getp(instr, Cfg)}.

	    
store_output_files(#job_desc{cfg=Cfg}, Log)->
    % close log and move it to storage
    logd:close(Log),
    JI = plist:getp(job_id, Cfg),
    Dom = plist:getp(job_domain, Cfg),
    LFName = lists:flatten(io_lib:format("~s.log", [JI])),
    PathLF = filename:join(wrfx_db:get_conf(workspace_root), LFName),
    wrfx_fstor:store({Dom, LFName}, PathLF).
    


format_time({Y,M,D}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D]).


retrieve_xls_files(Ds, Ss) ->
    lists:map(fun retrieve_xls_file/1, [{X, Y} || X <- Ds, Y <- Ss]).

retrieve_xls_file({{Y,M,D}, Code}) ->
    Name = lists:flatten(io_lib:format("~s-~4..0B--~2..0B-~2..0B", [Code, Y, M, D])),
    MesoDom = ?DOMAIN ++ io_lib:format("~4..0B-~2..0B-~2..0B", [Y,M,D]),
    case wrfx_fstor:exists({MesoDom, Name}) of
	{true, F} ->
	    F;
	false ->
	    URL = ?MWEST_DL_URL ++ construct_params(Y,M,D,Code),
	    {success, _} = tasks_net:http_sync_get(URL, "/tmp/wrfx-mwest-download"),
	    {success, F} = wrfx_fstor:store({MesoDom, Name}, "/tmp/wrfx-mwest-download"),
	    F
    end.

	    
    
construct_params(Y, M, D, Code) ->
    Pairs = lists:map( fun (I) -> io_lib:format("~s=~p", I) end,
		       [ ["product", "" ],
			 ["stn", Code ],
			 ["unit", "1"],
			 ["time", "GMT"],
			 ["day1", D],
			 ["month1", io_lib:format("~2..0B", [M])],
			 ["year1", Y],
			 ["hour1", "0"],
			 ["hours", 24],
			 ["daycalendar", 1],
			 ["output", "Excel"],
			 ["order", "0"] ]),
    string:join(Pairs, "&").
