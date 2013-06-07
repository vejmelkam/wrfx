
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
-export([check/1, execute/1, test_job/0]).

-compile(export_all).

-define(DOMAIN, "mwest").
-define(MWEST_DL_URL, "http://mesowest.utah.edu/cgi-bin/droman/meso_download_mesowest_ndb.cgi?").
-define(MWEST_INFO_URL, "http://mesowest.utah.edu/cgi-bin/droman/side_mesowest.cgi?stn=").

-compile(export_all).


test_job() ->
    Cfg = [ {from, {{2013,6,4}, {0,0,0}}},
	    {to,   {{2013,6,5}, {0,0,0}}},
	    {stations, ["ESPC2"]},
	    {wrfout, "/home/mvejmelka/Projects/wrfx/stor/outputs/"} ],
    J = #job_desc{key=test_moisture_job, cfg=Cfg},
    execute(J).


%% @doc
%% Checks whether the configuration structure is valid for wrf_job and makes
%% a best-effort check if this job will be executable.
%% @spec check(C::plist()) -> {success, []}|{failure, Reason}
%%
check(_J=#job_desc{cfg=_C}) ->
    {success, "configuration check success"}.


execute(J=#job_desc{cfg=Cfg}) ->
    [From, To, Ss] = plist:get_list([from, to, stations], Cfg),

    JI = lists:flatten("moisture_job_" ++ format_time(element(1, From))),

    % construct temporary workspaces from job name
    Wkspace = wrfx_db:get_conf(workspace_root),
    Dir = filename:join(Wkspace, JI),

    % ensure workspace is empty
    tasks_fsys:delete_dir(Dir),
    tasks_fsys:create_dir(Dir),

    Cfg2 = plist:update_with([{job_id, JI},
			      {job_domain, "outputs/" ++ JI},
			      {exec_dir, Dir},
			      {started, calendar:local_time()},
			      {instr, []}], Cfg),

    Log = logd:open([stdio, filename:join(Wkspace, "moisture_job.log")]),

    % retrieve all xls files from Mesowest (or from file cache)
    Ds = atime:dt_covering_days(From, To),
    Is = retrieve_info_files(Ss),
    Fs = retrieve_xls_files(Ds, Ss),

    % construct the observation variance table (substitute for actual variance estimates)
    Table = string:join(lists:map(fun ({V,Var}) -> V ++ ", " ++ Var end,
				  plist:getp(obs_var_table, Cfg)), "\n") ++ "\n",

    MCDir = filename:dirname(wrfx_db:get_conf(moisture_code_path)),

    Ts = [ {tasks_fsys, create_symlink, [F, filename:join(Dir, filename:basename(F))]} || F <- Fs ] ++
	 [ {tasks_fsys, create_symlink, [F, filename:join(Dir, filename:basename(F))]} || F <- Is ] ++
	 [ {tasks_fsys, write_file, [filename:join(Dir, "obs_var_table"), Table]},
	   {tasks_fsys, write_file, [filename:join(Dir, "station_list"), string:join(Ss, "\n") ++ "\n"]},
	   {tasks_fsys, write_file, [filename:join(MCDir, "rda.cfg"), make_config_file(Cfg2)]},
	   {tasks_exec, execute, [wrfx_db:get_conf(scraper_path),
				  [ {in_dir, Dir},
				    {output_type, stdout},
				    {op_args, [{args, ["station_list", "obs_var_table"]}]},
				    {store_output_to, filename:join(Dir, "extract_observations.log")},
				    {exit_check, exit_code}]]},
	   {tasks_fsys, delete_files_regexp, [Dir, ".*\.xls"]},
	   {tasks_exec, execute, [wrfx_db:get_conf(moisture_code_path),
				  [ {in_dir, MCDir},
				    {output_type, stdout},
				    {op_args, [{args, ["rda.cfg"]}]},
				    {store_output_to, filename:join(Dir, "moisture_code.log")},
				    {exit_check, exit_code}]]},
	   {tasks_exec, delete_files_regexp, [Dir, ".*\.obs"]} ],
    
    Plan = #plan{id=prep_moisture_run, tasks=Ts},
    PL = plan_logger:start(Log),
    PID = plan_runner:execute_plan(Plan, [PL]),
    case plan_runner:wait_for_plan(PID) of
	{success, _} ->
	    post_moisture_run(J#job_desc{cfg=Cfg2}, Log);
	R ->
	    job_failed(prep_moisture_run, J#job_desc{cfg=Cfg2}, Log, R)
    end.


post_moisture_run(J=#job_desc{key=JK, cfg=Cfg}, Log) ->

    store_output_files(J, Log),

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
    Name = lists:flatten(io_lib:format("~s_~4..0B-~2..0B-~2..0B.xls", [Code, Y, M, D])),
    MesoDom = ?DOMAIN ++ io_lib:format("/~4..0B-~2..0B-~2..0B", [Y,M,D]),
    case wrfx_fstor:exists({MesoDom, Name}) of
	{true, F} ->
	    F;
	false ->
	    URL = lists:flatten([?MWEST_DL_URL, construct_params(Y,M,D,Code)]),
	    {success, _} = tasks_net:http_sync_get(URL, "/tmp/wrfx-mwest-download"),
	    {success, F} = wrfx_fstor:store({MesoDom, Name}, "/tmp/wrfx-mwest-download"),
	    F
    end.
	    
    
construct_params(Y, M, D, Code) ->
    Pairs = lists:map( fun (I) -> io_lib:format("~s=~s", I) end,
		       [ ["product", "" ],
			 ["stn", Code ],
			 ["unit", "1"],
			 ["time", "GMT"],
			 ["day1", integer_to_list(D)],
			 ["month1", lists:flatten(io_lib:format("~2..0B", [M]))],
			 ["year1", integer_to_list(Y)],
			 ["hour1", "0"],
			 ["hours", "24"],
			 ["daycalendar", "1"],
			 ["output", "Excel"],
			 ["order", "0"],
			 ["TMPF", "TMPF"],
			 ["RELH", "RELH"],
			 ["FM", "FM"],
			 ["PREC", "PREC"],
		         ["QFLG", "QFLG"] ]),
    string:join(Pairs, "&").


make_config_file(Cfg) ->
    Dir = plist:getp(exec_dir, Cfg),
    QDir = "\"" ++ Dir ++ "\"",
    Pairs = [ {"station_info_dir", QDir },
	      {"station_data_dir", QDir },
	      {"station_info", "\"station_list\""},
	      {"output_dir", QDir},
	      {"wrf_output", "\"" ++ plist:getp(wrfout, Cfg) ++ "\""},
	      {"Q", "[2.0e-4, 1.0e-4, 5.0e-5, 0, 0, 0, 5.0e-5, 5.0e-5, 0]"},
	      {"P0", "[0.01, 0.01, 0.01, 0, 0, 0, 0.01, 0.01, 0]"},
	      {"covariates", "[:constant, :temperature, :pressure, :rain, :lon, :lat, :elevation]"},
	      {"assimilation_time_window", "3600"} ],
    PS = lists:map(fun ({X,Y}) -> io_lib:format("~p => ~s", [X, Y]) end, Pairs),
    "[\n" ++ string:join(PS, ",\n") ++ "\n]\n".
    
		      
retrieve_info_files(Ss) ->
    lists:map(fun retrieve_info_file/1, Ss).

retrieve_info_file(Code) ->
    Name = Code ++ ".info",
    MesoDom = ?DOMAIN ++ "/info",
    case wrfx_fstor:exists({MesoDom, Name}) of
	{true, F} ->
	    F;
	false ->
	    {success, _Msg, Bdy} = tasks_net:http_sync_get(?MWEST_INFO_URL ++ Code),
	    io:format("~p~n", [Bdy]),
	    Vals = extract_values_from_body(Bdy, [<<"NAME:">>, <<"LATTITUDE:">>, <<"LONGITUDE:">>, <<"ELEVATION:">>], []),
	    file:write_file("/tmp/info_file", string:join([Code|Vals], "\n")),
	    {success, F} = wrfx_fstor:store({MesoDom, Name}, "/tmp/info_file"),
	    F
    end.
	    
	    
extract_values_from_body(Bdy, [V|Vs], C) ->
    [Line, Rest] = binary:split(Bdy, <<"<br />">>),
    case binary:split(Line, V) of
	[_Prefix, Value] ->
	    % somewhat cumbersome but remove spaces and eols from string
	    Val = extract_value(V, string:strip(string:strip(binary_to_list(Value)), both, $\n)),
	    extract_values_from_body(Rest, Vs, [Val|C]);
	[_NoSplit] ->
	    extract_values_from_body(Rest, [V|Vs], C)
    end.


extract_value(<<"ELEVATION:">>, Val) ->
    [Num, _Ft] = string:tokens(Val, " "),
    io_lib:format("~p", [list_to_integer(Num) * 0.3048]);
extract_value(_V, Val) ->
    Val.



