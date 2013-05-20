

-module(stor).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-behavior(gen_server).

-export([ensure_location/2, resolve/2, check_exists/2, remove/2, store/3,  % API
	 init/1, handle_call/3, handle_cast/2,  handle_info/2, terminate/2, code_change/3,
	 start_link/0, start/0, stop_storage/0]).

start() ->
    application:start(stor).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_Args) ->
    {ok, Root} = cfg:get_conf(storage_root),
    ok = filelib:ensure_dir(filename:join(Root, "touch")),
    {ok, Root}.


handle_call({store, Domain, ID, File}, _From, Root) ->
    Dest = filename:join([Root, Domain, ID]),
    case file:rename(File, Dest) of
	ok ->
	    {reply, success, Root};
	{error, R} ->
	    {reply, {failure, R}, Root}
    end;

handle_call({ensure_location, Domain, ID}, _From, Root) ->
    F = filename:join([Root, Domain, ID]),
    case filelib:ensure_dir(F) of
	ok ->
	    {reply, {success, F}, Root};
	{error, _E} ->
	    {reply, {failure, F}, Root}
    end;

handle_call({remove, Dom, ID}, _From, Root) ->
    F = filename:join([Root, Dom, ID]),
    case file:delete(F) of
	ok ->
	    {reply, success, Root};
	{error, R} ->
	    {reply, {failure, R}, Root}
    end;

handle_call({resolve, Domain, ID}, _From, Root) ->
    F = filename:join([Root, Domain, ID]),
    {reply, F, Root};

handle_call({check_exists, Domain, ID}, _From, Root) ->
    F = filename:join([Root, Domain, ID]),
    case filelib:is_file(F) of
	true ->
	    {reply, F, Root};
	false ->
	    {reply, not_found, Root}
    end;


handle_call(terminate, _From, _State) ->
    {stop, normal, ok, no_storage}.
    

terminate(normal, _State) ->
    ok.


code_change(_Old, State, _Extra) ->
    {ok, State}.
    

handle_info(Info, State) ->
    io:format("Unexpected message ~p~n", [Info]),
    {noreply, State}.


handle_cast(_Req, State) ->
    {noreply, State}.



store(Dom, ID, File) ->
    gen_server:call(stor, {store, Dom, ID, File}).

ensure_location(Dom, ID) ->
    gen_server:call(stor, {ensure_location, Dom, ID}).


resolve(Dom, ID) -> 
    gen_server:call(stor, {resolve, Dom, ID}).


check_exists(Dom, ID) ->
    gen_server:call(stor, {check_exists, Dom, ID}).

remove(Dom, ID) ->
    gen_server:call(stor, {remove, Dom, ID}).

stop_storage() ->
    gen_server:call(stor, terminate).
