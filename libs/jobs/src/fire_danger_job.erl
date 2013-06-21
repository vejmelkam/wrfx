
%% @doc
%% This is a compound job that runs first the wrf_job
%% and then the moisture_job on the result of the wrf_job
%% and then post_processes the result to insert it into
%% riak as PNGs.
%%
%%
%% Input plist must contain:
%%
%% wrf_job_id - the id of the wrf_job to run (stored in the dbase)
%% moisture_job_id - the id of the moisture_job to run (stored in the dbase)
%% wrfout_base - the base part (without the date) of the wrfout to use for assimilation (e.g. "wrfout_d01_")
%% vars - string containing a comma separated list (no spaces) of the vars to render/upload to RIAK
%% riak_host - hostname of riak cluster
%% riak_port - port of pbc protocol on riak cluster
%%
-module(fire_danger_job).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-include_lib("jobs/include/jobs.hrl").
-include_lib("flow/include/flow.hrl").
-export([id/1, check/1, execute/1]).



%% @doc
%% Constructs and returns an id for this job.
%% @spec id(job_desc()) -> string()
%%
id(#job_desc{key=JK}) ->
    {FromDate, _FromTime} = calendar:local_time(),
    lists:flatten([atom_to_list(JK), "_", format_time(FromDate)]).


%% @doc
%% Checks whether the configuration structure is valid for wrf_job and makes
%% a best-effort check if this job will be executable.
%% Now is a stub.
%% @spec check(job_desc()) -> {success, []}|{failure, Reason}
%% 
check(#job_desc{}) ->
    {success, []}.


%% @doc
%% Executes the wrf_job first and if successful, the moisture job.
%% @spec execute(job_desc()) -> job_report()
%%
execute(J=#job_desc{cfg=Cfg}) ->
    
    {ok, CWD} = file:get_cwd(),

    {success, WJ=#job_desc{cfg=Wcfg}} = wrfx_db:lookup({job_desc, plist:getp(wrf_job_id, Cfg)}),
    {success, MJ=#job_desc{cfg=Mcfg}} = wrfx_db:lookup({job_desc, plist:getp(moisture_job_id, Cfg)}),

    WrfoutBase = plist:getp(wrfout_base, Cfg),
    {From, To} = wrf_job:extract_interval(Wcfg),
    FromStr = esmf:time_to_string(From),

    Mcfg2 = plist:update_with([{from, From},
			       {to, To},
			       {wrfout, filename:join([CWD, "stor", "outputs", wrf_job:id(WJ), lists:flatten([WrfoutBase, FromStr])])}],
			      Mcfg),

    JI = id(J),

    
    Cfg2 = plist:update_with([{moisture_job_desc, MJ#job_desc{cfg=Mcfg2}},
			      {started, calendar:local_time()},
			      {job_id, JI}], Cfg),

    % open a logger that will record all plan activity
    Wkspace = wrfx_db:get_conf(workspace_root),
    Log = logd:open([stdio, filename:join(Wkspace, lists:flatten([JI, ".log"]))]),
    logd:message("[FD] [STAGE] running wrf job", Log),
    case wrf_job:execute(WJ) of
	#job_report{result = {success, _}} ->
	    logd:message("[FD] [STAGE] running moisture job", Log),
	    run_moisture_job(J#job_desc{cfg=Cfg2}, Log);
	Failure ->
	    job_failed(wrf_job, J, Failure, Log)
    end.


run_moisture_job(J=#job_desc{cfg=Cfg}, Log) ->
    logd:message("[FD] [STAGE] running moisture code", Log),
    MJ  = plist:getp(moisture_job_desc, Cfg),
    case moisture_job:execute(MJ) of
	#job_report{result = {success, _}} ->
	    postprocess(J, Log);
	Failure ->
	    job_failed(moisture_code, J, Failure, Log)
    end.


postprocess(J=#job_desc{key=JK, cfg=Cfg}, Log) ->
    logd:message("[FD] [STAGE] running postprocess job", Log),

    Vars = plist:getp(vars, Cfg),
    Bucket = JK,
    RiakHost = plist:getp(riak_host, Cfg),
    RiakPort = plist:getp(riak_port, Cfg),
    #job_desc{cfg=Mcfg}  = plist:getp(moisture_job_desc, Cfg),
    Wrfout = plist:getp(wrfout, Mcfg),

    {ok, CWD} = file:get_cwd(),
    Dir = filename:join([CWD, "deps", "postproc"]),

    T = [ {tasks_exec, execute, [ filename:join([Dir, "postproc_wrfout.py"]),
				  [ {in_dir, Dir},
				    {output_type, stdout},
				    {op_args, [{args, ["-v", Vars, "-b", Bucket, "-n", RiakHost, "-p", RiakPort, Wrfout, "."]}]},
				    {store_output_to, filename:join(Dir, "postproc_wrfout.log")},
				    {exit_check, exit_code}]]} |
	  [ {tasks_fsys, delete_file, [filename:join([Dir, X])]} ||
	      X <- filesys:list_dir_regexp(Dir, ".*png") ] ],

    Plan = #plan{id = fd_postprocess, tasks = T},
    PL = plan_logger:start(Log),
    PID = plan_runner:execute_plan(Plan, [PL]),
    case plan_runner:wait_for_plan(PID) of
	{success, _} ->
	    logd:message("[FD] [SUCCESS] complete", Log),
	    #job_report{job_id = plist:getp(job_id, Cfg),
			job_desc_key = JK,
			started = plist:getp(started, Cfg),
			completed = calendar:local_time(),
			result = {success, "SUCCESS"},
			wksp_dir = [],
			stor_dom = [],
			log_stor_id = {},
			inst = [] };
	R ->
	    job_failed(postprocess, J, Log, R)
    end.
				  
    

job_failed(A, F, #job_desc{key=JK, cfg=Cfg}, Log) ->

    logd:message(io_lib:format("[FD] [FAILED] in stage ~p", [A]), Log),
    logd:close(Log),

    % return a job report
    #job_report{job_id = plist:getp(job_id, Cfg),
		job_desc_key = JK,
		started = plist:getp(started, Cfg),
		completed = calendar:local_time(),
		result = F,
		wksp_dir = [],
		stor_dom = [],
		log_stor_id = {},
		inst = plist:getp(instr, Cfg)}.


format_time({Y,M,D}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D]).
