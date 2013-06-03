

-module(sched).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/1, stop/1, jstat/1, jdesc/1]).

-include_lib("jobs/include/jobs.hrl").

start(J=#job_desc{}) ->
    spawn(fun() -> wait_loop(J) end);

start(Jid) ->
    {success, J} = wrfx_db:lookup({job_desc, Jid}),
    start(J).


jstat(PID) ->
    PID ! {self(), status},
    receive
	{PID, JS} ->
	    JS
    after 1000 ->
	    timeout
    end.

stop(PID) ->
    PID ! {self(), stop},
    receive
	{PID, stopped} ->
	    ok
    after 1000 ->
	timeout
    end.


jdesc(PID) ->
    PID ! {self(), get_job_desc},
    receive
	{PID, job_desc, J} ->
	    J
    after 1000 ->
	timeout
    end.		


wait_loop(J=#job_desc{id = Id, cfg=C}) ->
    Sched = plist:getp(schedule, C),
    {_D, TNow} = calendar:universal_time(),
    T = time_to_next_run_sec(Sched, TNow),
    receive
	{From, status} ->
	    {_D2, TNow2} = calendar:universal_time(),
	    JA = #job_activity{id = Id,
			       status = {waiting, time_to_next_run_sec(Sched, TNow2)}},
	    From ! {self(), JA},
	    wait_loop(J);
	{From, stop} ->
	    From ! {self(), stopped};
	{From, get_job_desc} ->
	    From ! {self(), job_desc, J},
	    wait_loop(J)
    after T * 1000 ->
	    {M, F} = plist:getp(mf, C),
	    S = self(),
	    PID = spawn(fun () -> S ! {self(), job_done, M:F(J)} end),
	    run_loop(J, calendar:local_time(), PID)
    end.


run_loop(J=#job_desc{id=Id, cfg=C}, StartTime, PID) ->
    S = plist:getp(schedule, C),
    receive
	{From, status} ->
	    JA = #job_activity{id = Id,
			       status = {running, StartTime}},
	    From ! {self(), JA},
	    run_loop(J, StartTime, PID);
	{From, stop} ->
	    % @TODO: job should be killed here
	    From ! {self(), stopped};
	{From, get_job_desc} ->
	    From ! {self(), job_desc, J},
	    wait_loop(J);
	{PID, job_done, JR} ->
	    % store job report in database
	    wrfx_db:store(JR),
	    case S of
		now ->
		    JR;
		_ ->
		    wait_loop(J)
	    end
    end.
		

time_to_next_run_sec(now, _T) ->
    0;
time_to_next_run_sec({JH, JM, JS}, {H, M, S}) ->
    T = (JH - H) * 3600 + (JM - M) * 60 + (JS - S),
    case T < 0 of
	true ->
	    T + 24*3600;
	false ->
	    T
    end.


