

%% @doc
%%  An incipient message router.  Currently unused in the application except for
%% the {@link multicast/2} function, which allows sending messages to multiple
%% targets.


-module(msg_router).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([send/2, subscribe/1, unsubscribe/1,
	start/0, multicast/2]).


send(EvType, Msg) ->
    notify_server ! {msg, EvType, Msg}.

subscribe(EvType) ->
    notify_server ! {subscribe, self(), EvType}.

unsubscribe(EvType) ->
    notify_server ! {unsubscribe, self(), EvType}.


start() ->
    PID = spawn(fun() -> notify_server_loop(dict:new()) end),
    register(routing_server, PID).


notify_server_loop(Subs) ->
    receive
	stop ->
	    unregister(routing_server),
	    ok;
	{msg, AboutWhat, Msg} ->
	    L = dict:fetch(AboutWhat, Subs),
	    multicast(Msg, L),
	    notify_server_loop(Subs);
	{subscribe, Who, AboutWhat} ->
	    notify_server_loop(dict:append(AboutWhat, Who, Subs));
	{unsubscribe, Who, AboutWhat} ->
	    notify_server_loop(dict:update(AboutWhat, fun (L) -> lists:delete(Who, L) end, Subs));
	M ->
	    io:format("message router: dropping unrecognized message ~p~n", [M]),
	    notify_server_loop(Subs)
    end.


multicast(_M, []) ->
    ok;
multicast(M, [T|L]) ->
    T ! M,
    multicast(M, L).
