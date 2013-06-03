

%% @doc
%% This job runs on a given wrfout file.
%%

-module(moisture_job).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-include_lib("jobs/include/jobs.hrl").
-include_lib("flow/include/flow.hrl").
-export([check/1, execute/1]).

%% @doc
%% Checks whether the configuration structure is valid for wrf_job and makes
%% a best-effort check if this job will be executable.
%% @spec check_config(C::plist()) -> {success, []}|{failure, Reason}
%%
check(J=#job_desc{cfg=_C}) ->
    {success, "configuration check success"}.


execute(J=#job_desc{cfg=Cfg}) ->
    [Dir, From, To, Ss] = plist:get_list([wrf_exec_dir, wrf_from, wrf_to, stations], Cfg),
    SDir = wrfx_db:get_conf(scraper_dir),

    Log = logd:open([stdio, filename:join(Dir, "moisture_job.log")]),

    % retrieve all xls files from Mesowest (or from file cache)
    Ds = lists:map(fun format_time/1, atime:dt_covering_days(From, To)),
%    retrieve_xls_files(Ds, Ss),

    % convert to obs files
%    file:write(filename:join(Dir, "station_list"), string:join(Ss, "\n")),
%    write_obs_table(Dir, plist:getp(obs_table, Cfg)),

    % @TODO: add xls -> obs files code here

    T = [ {tasks_exec, execute, [ filename:join(SDir, "scrape_stations.py"),
				  [{in_dir, Dir},
				   {exit_check, exit_code},
				   {op_args, [{args, ["-f", "xls",
						      "-c", C,
						      "-i", "24",
						      "-t", D,
						      "dl"]}]}] ]} || D <- Ds, C <- Ss ],
    Plan = #plan{id=fm_data_download, tasks=T},

    PL = plan_logger:start(Log),
    PID = plan_runner:execute_plan(Plan, [PL]),
    case plan_runner:wait_for_plan(PID) of
	{success, _} ->
	    {success, []};
	R ->
	    R
    end.
	    
format_time({Y,M,D}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B_00:00", [Y, M, D]).
