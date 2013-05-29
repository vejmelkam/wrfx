

-module(cfg).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-behavior(gen_server).

-export([get_conf/1, stop_cfg/0,
	 init/1, handle_call/3, handle_cast/2,  handle_info/2, terminate/2, code_change/3,
	 start_link/0, start/0]).


start() ->
    application:start(cfg).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_Args) ->
    {ok, [[H]]} = init:get_argument(home),
    C = filename:join(H, ".wrfxcfg"),
    {ok, init_state(C)}.


handle_call({get_conf, Key}, _From, C) ->
    case plist:contains(Key, C) of
	true ->
	    {reply, {ok, plist:getp(Key, C)}, C};
	false ->
	    {reply, {error, not_found}, C}
    end;

handle_call(terminate, _From, _State) ->
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



get_conf(Key) ->
    gen_server:call(cfg, {get_conf, Key}).


stop_cfg() ->
    gen_server:call(cfg, terminate).


init_state(C) ->
    case filelib:is_file(C) of
	true ->
	    load_terms(C);
	false ->
	    default_state()
    end.



load_terms(C) ->
    case file:consult(C) of
	{ok, Terms} ->
	    Terms;
	{error, _} ->
	    default_state()
    end.


default_state() ->
    {ok, [[H]]} = init:get_argument(home),
    [ {stor_root, filename:join(H, "wrfx_stor")},
      {workspace_root, filename:join(H, "workspace")} ].
