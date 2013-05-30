

-module(wrfx_jobmgr).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-define(SERVER, ?MODULE).

-include_lib("util/include/job_desc.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([list_jobs/0, add_job/1, remove_job/1, get_job_pid/1]).

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
list_jobs() ->
    gen_server:call(?SERVER, list_jobs).

%% @spec add_job(J::job_desc()) -> success | failure.
add_job(J) ->
    wrfx_db:store(J),
    gen_server:call(?SERVER, {add_job, J}).

%% @spec remove_job(Jid::term()) -> success | failure.
remove_job(Jid) ->
    {success, PID} = get_job_pid(Jid),
    J = sched:job_desc(PID),
    wrfx_db:delete(J),
    gen_server:call(?SERVER, {remove_job, PID}).

%% @spec get_job(Jid::term()) -> {success, pid()} | failure
get_job_pid(Jid) ->
    gen_server:call(?SERVER, {get_job_pid, Jid}).
    

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    Js = wrfx_db:all(job_desc),
    Infos = lists:map(fun start_job/1, Js),
    {ok, Infos}.


handle_call(list_jobs, _From, Infos) ->
    {reply, plist:keys(Infos), Infos};

handle_call({add_job, J}, _From, Infos) ->
    Info = start_job(J),
    {reply, ok, [Info|Infos]};

handle_call({remove_job, Jid}, _From, Infos) ->
    PID = plist:getp(Jid, Infos),
    success = sched:stop(PID),
    {reply, success, plist:remove_key(Jid, Infos)};

handle_call({get_job_pid, Jid}, _From, Infos) ->
    PID = plist:getp(Jid, Infos),
    {reply, {success, PID}, Infos};

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

