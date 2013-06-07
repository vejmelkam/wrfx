
-module(wrfx_jadm).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-define(SERVER, ?MODULE).

-include_lib("jobs/include/jobs.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([jobs/0, job_stats/0, run_job/1, schedule_job/1, cancel_job/1, get_scheduler/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% @spec jobs() -> [pid()]
%% @doc lists all jobs loaded into the job admin.  note: not all jobs are running
jobs() ->
    gen_server:call(?SERVER, list_jobs).


%% @spec job_stats() -> [job_activity()]
%% @doc queries and reports the activity of all jobs registered with jadm.
job_stats() ->
    gen_server:call(?SERVER, job_stats).


%% @spec run_job(J::job_desc()) -> success | failure
%% @doc run a job now --- sets schedule to now to ensure scheduler executes job right away
run_job(J=#job_desc{cfg=C}) ->
    C2 = plist:setp(schedule, now, C),
    gen_server:call(?SERVER, {schedule_job, J#job_desc{cfg=C2}}).


%% @spec schedule_job(J::job_desc()) -> ok
%% @doc schedules the job, the job is run at the time specified by the schedule config.
schedule_job(J) ->
    gen_server:call(?SERVER, {schedule_job, J}).


%% @spec cancel_job(Jid::term()) -> success | failure
%% @doc removes a job from scheduling, note that if a job is running, it is not killed.
cancel_job(Jid) ->
    case get_scheduler(Jid) of
	{success, PID} ->
	    gen_server:call(?SERVER, {cancel_job, PID});
	{failure, E} ->
	    {failure, E}
    end.


%% @spec get_scheduler(Jid::term()) -> {success, pid()} | failure
%% @doc retrieves the PID of the scheduler that is running the job Jid.
get_scheduler(Jid) ->
    gen_server:call(?SERVER, {get_pid_for_jid, Jid}).
    

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    Js = wrfx_db:all(job_desc),
    AutoJs = lists:filter(fun (#job_desc{cfg=C}) -> plist:getp(auto_start, C) end, Js),
    Infos = lists:map(fun start_job/1, AutoJs),
    {ok, Infos}.

handle_call(list_jobs, _From, Infos) ->
    {reply, plist:keys(Infos), Infos};

handle_call(job_stats, _From, Infos) ->
    {reply, lists:map(fun ({_Jid,PID}) -> sched:status(PID) end, Infos), Infos};

handle_call({schedule_job, J}, _From, Infos) ->
    Info = start_job(J),
    {reply, ok, [Info|Infos]};

handle_call({remove_job, Jid}, _From, Infos) ->
    PID = plist:getp(Jid, Infos),
    success = sched:stop(PID),
    {reply, success, plist:remove_key(Jid, Infos)};

handle_call({get_pid_for_jid, Jid}, _From, Infos) ->
    case plist:getp(Jid, Infos, not_found) of
	not_found ->
	    {reply, {failure, not_found}, Infos};
	PID ->
	    {reply, {success, PID}, Infos}
    end;

handle_call(terminate, _From, Infos) ->
    lists:foreach(fun ({_Jid, PID}) -> success = sched:stop(PID) end, Infos),
    {stop, normal, ok, []}.

    
terminate(normal, _State) ->
    ok.


code_change(_Old, State, _Extra) ->
    {ok, State}.
    

handle_info(Info, State) ->
    io:format("Unexpected message ~p~n", [Info]),
    {noreply, State}.


handle_cast(_Req, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


start_job(J=#job_desc{key=JK}) ->
    PID = sched:start(J),
    {JK, PID}.

