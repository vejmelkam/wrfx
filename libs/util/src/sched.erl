

-module(sched).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([wait_loop/1, start/1, status/1]).



start(C) ->
    spawn(fun() -> wait_loop(C) end).


wait_loop(C) ->
    Sched = plist:getp(schedule, C),
    {_D, TNow} = calendar:universal_time(),
    T = time_to_next_run_sec(Sched, TNow),
    io:format("wait_loop: waiting ~p seconds to run~n", [T]),
    receive
	{From, status} ->
	    {_D2, TNow2} = calendar:universal_time(),
	    From ! {self(), waiting, time_to_next_run_sec(Sched, TNow2)},
	    wait_loop(C);
	{From, exit} ->
	    From ! {self(), stopped},
	    exited
    after T * 1000 ->
	    {M, F} = plist:getp(mf, C),
	    S = self(),
	    PID = spawn(fun () -> S ! {self(), job_done, M:F(C)} end),
	    io:format("wait_loop: starting job ~p:~p, PID is ~p", [M, F, PID]),
	    run_loop(C, PID)
    end.


run_loop(C, PID) ->
    receive
	{From, status} ->
	    From ! {self(), running},
	    run_loop(C, PID);
	{From, exit} ->
	    From ! {self(), exiting},
	    exited;
	{PID, job_done, _R} ->
	    wait_loop(C)
    end.
		

time_to_next_run_sec({JH, JM, JS}, {H, M, S}) ->
    T = (JH - H) * 3600 + (JM - M) * 60 + (JS - S),
    case T < 0 of
	true ->
	    T + 24*3600;
	false ->
	    T
    end.


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
