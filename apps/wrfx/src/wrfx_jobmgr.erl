

-module(wrfx_jobmgr).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-define(SERVER, ?MODULE).

-include_lib("util/include/job_desc.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([active_jobs/0, run_job/1, schedule_job/1, cancel_job/1, get_job_pid/1]).

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


%% @spec jobs(pid()) -> [pid()]
active_jobs() ->
    gen_server:call(?SERVER, list_jobs).

%% @spec add_job(J::job_desc()) -> success | failure.
run_job(J=#job_desc{cfg=C}) ->
    C2 = plist:setp(schedule, now, C),
    gen_server:call(?SERVER, {schedule_job, J#job_desc{cfg=C2}}).

schedule_job(J) ->
    gen_server:call(?SERVER, {schedule_job, J}).
    
%% @spec remove_job(Jid::term()) -> success | failure.
cancel_job(Jid) ->
    case get_job_pid(Jid) of
	{success, PID} ->
	    gen_server:call(?SERVER, {cancel_job, PID});
	{failure, E} ->
	    {failure, E}
    end.

%% @spec get_job(Jid::term()) -> {success, pid()} | failure
get_job_pid(Jid) ->
    gen_server:call(?SERVER, {get_job_pid, Jid}).
    

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

handle_call({schedule_job, J}, _From, Infos) ->
    Info = start_job(J),
    {reply, ok, [Info|Infos]};

handle_call({remove_job, Jid}, _From, Infos) ->
    PID = plist:getp(Jid, Infos),
    success = sched:stop(PID),
    {reply, success, plist:remove_key(Jid, Infos)};

handle_call({get_job_pid, Jid}, _From, Infos) ->
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


start_job(J=#job_desc{id=Id}) ->
    PID = sched:start(J),
    {Id, PID}.

