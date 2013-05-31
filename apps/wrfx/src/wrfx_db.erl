

%% @doc
%% An interface module to mnesia storage.  First version is a thin wrapper
%% without use of transactions.
%%


-module(wrfx_db).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-define(SERVER, ?MODULE).

-export([start/0, stop/0]).
-export([store/1, lookup/1, lookup/2, list_keys/1, all/1, delete/1]).
-export([get_conf/1, set_conf/2]).

%% @doc Starts mnesia if not already running and ensures that
%% tables required for system operations are available.
%% @spec start() -> ok
start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    ensure_tables(),
    mnesia:wait_for_tables([wrfx_cfg], 5000).

%% @doc Stops mnesia.
stop() ->
    mnesia:stop().


get_conf(K) ->
    {success, {wrfx_cfg, K, V}} = lookup({wrfx_cfg, K}),
    V.

set_conf(K, V) ->
    store({wrfx_cfg, K, V}).

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
	Res = mnesia:dirty_read({element(1, R), element(N, R)}),
	case Res of
	    [V] ->
		{success, V};
	    [] ->
		{failure, not_found}
	end
    catch
	{'EXIT', {aborted, R}} ->
	    {failure, R}
    end.


list_keys(T) ->
    mnesia:dirty_all_keys(T).

all(T) ->
    {atomic, R} = mnesia:transaction(fun () -> mnesia:foldl(fun (X, A) -> [X|A] end, [], T) end, 1),
    R.


wrap_dirty_fun(F) ->
    try
	ok = F(),
	success
    catch
	{'EXIT', {aborted, R}} ->
	    {failure, R}
    end.
	

ensure_tables() ->
    lists:foreach(fun ensure_table/1, [nllist, wrfx_cfg, fields, job_desc]),
    ok.


ensure_table(T) ->
    Ts = mnesia:system_info(tables),
    case lists:member(T, Ts) of
	true ->
	    ok;
	false ->
	    {atomic, ok} = mnesia:create_table(T, [ {access_mode, read_write}, {disc_copies, [node()]}, {type, set} ])
    end.
