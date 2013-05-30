

-module(sched).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/1, stop/1, status/1, job_desc/1]).

-include_lib("util/include/job_desc.hrl").

start(J) ->
    spawn(fun() -> wait_loop(J) end).


status(PID) ->
    PID ! {self(), status},
    receive
	{PID, running} ->
	    running;
	{PID, waiting, T} ->
	    {waiting, T}
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


job_desc(PID) ->
    PID ! {self(), get_job_desc},
    receive
	{PID, job_desc, J} ->
	    J
    after 1000 ->
	timeout
    end.		


wait_loop(J=#job_desc{id=Id, cfg=C}) ->
    Sched = plist:getp(schedule, C),
    {_D, TNow} = calendar:universal_time(),
    T = time_to_next_run_sec(Sched, TNow),
    io:format("wait_loop: waiting ~p seconds to run~n", [T]),
    receive
	{From, status} ->
	    {_D2, TNow2} = calendar:universal_time(),
	    From ! {self(), waiting, time_to_next_run_sec(Sched, TNow2)},
	    wait_loop(J);
	{From, stop} ->
	    From ! {self(), stopped},
	    success;
	{From, get_id} ->
	    {self(), id, Id}
    after T * 1000 ->
	    {M, F} = plist:getp(mf, C),
	    S = self(),
	    PID = spawn(fun () -> S ! {self(), job_done, M:F(C)} end),
	    io:format("wait_loop: starting job ~p:~p, PID is ~p", [M, F, PID]),
	    run_loop(J, PID)
    end.


run_loop(J, PID) ->
    receive
	{From, status} ->
	    From ! {self(), running},
	    run_loop(J, PID);
	{From, exit} ->
	    From ! {self(), exiting},
	    exited;
	{PID, job_done, _R} ->
	    wait_loop(J)
    end.
		

time_to_next_run_sec({JH, JM, JS}, {H, M, S}) ->
    T = (JH - H) * 3600 + (JM - M) * 60 + (JS - S),
    case T < 0 of
	true ->
	    T + 24*3600;
	false ->
	    T
    end.


