

%% @doc
%% An interface module to mnesia storage.  First version is a thin wrapper
%% without use of transactions.
%%


-module(wrfx_db).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-define(SERVER, ?MODULE).

-export([start/0, stop/0]).
-export([store/1, lookup/1, lookup/2, list/1, delete/1]).


%% @doc Starts mnesia if not already running and ensures that
%% tables required for system operations are available.
%% @spec start() -> ok
start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    ensure_tables().

%% @doc Stops mnesia.
stop() ->
    mnesia:stop().


%% @doc Stores the record in a table with the same name as the record.
%% @spec store(R::tuple()) -> success | {failure, Reason}
store(R) ->
    wrap_dirty_fun(fun() -> mnesia:dirty_write(R) end).

delete(R) ->
    wrap_dirty_fun(fun() -> mnesia:dirty_delete(R) end).

lookup(R) ->
    lookup(R,2).

lookup(R,N) when N > 1 ->
    try
	[V] = mnesia:dirty_read({element(1, R), element(N, R)}),
	{success, V}
    catch
	{'EXIT', {aborted, R}} ->
	    {failure, R}
    end.


list(T) ->
    try
	Ks = mnesia:dirty_all_keys(T),
	{success, Ks}
    catch
	{'EXIT', {aborted, R}} ->
	    {failure, R}
    end.


wrap_dirty_fun(F) ->
    try
	F,
	success
    catch
	{'EXIT', {aborted, R}} ->
	    {failure, R}
    end.
	

ensure_tables() ->
    lists:foreach(fun ensure_table/1, [nllist, cfg]),
    ok.


ensure_table(T) ->
    Ts = mnesia:system_info(tables),
    case lists:member(T, Ts) of
	true ->
	    ok;
	false ->
	    {atomic, ok} = mnesia:create_table(T, [ {access_mode, read_write}, {disc_copies, [node()]}, {type, set} ])
    end.
