

-module(wrfx_fstor).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-behavior(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([resolve/1, exists/1, remove/1, store/2, list_domain/1]).  % file API
-export([stop_storage/0]).

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

%% @type id() = {string(), string()}

%% @spec file_store(ID::id(), File::string()) -> {success, Path} | {failure, Reason}
store(ID, File) ->
    gen_server:call(?SERVER, {file_store, ID, File}).

%% @spec file_resolve(ID::id()) -> string()
resolve(ID) -> 
    gen_server:call(?SERVER, {file_resolve, ID}).

%% @spec file_exists(ID::id()) -> {true, F} | false
exists(ID) ->
    gen_server:call(?SERVER, {file_exists, ID}).

%% @spec file_remove(ID::id()) -> success | {failure, R}
remove(ID) ->
    gen_server:call(?SERVER, {file_remove, ID}).

list_domain(Dom) ->
    gen_server:call(?SERVER, {list_domain, Dom}).

stop_storage() ->
    gen_server:call(?SERVER, terminate).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, Root} = wrfx_cfg:get_conf(storage_root),
    ok = filelib:ensure_dir(filename:join(Root, "touch")),
    {ok, Root}.


handle_call({file_store, {Dom, Name}, F}, _From, Root) ->
    Dst = filename:join([Root, Dom, Name]),
    case filelib:ensure_dir(Dst) of
	ok ->
	    R = move_or_copy(F, Dst),
	    {reply, R, Root};
	{error, E} ->
	    {reply, {failure, E}, Root}
    end;

handle_call({file_remove, {Dom, Name}}, _From, Root) ->
    F = filename:join([Root, Dom, Name]),
    case file:delete(F) of
	ok ->
	    {reply, success, Root};
	{error, R} ->
	    {reply, {failure, R}, Root}
    end;

handle_call({file_resolve, {Dom, Name}}, _From, Root) ->
    F = filename:join([Root, Dom, Name]),
    {reply, F, Root};

handle_call({file_exists, {Dom, Name}}, _From, Root) ->
    F = filename:join([Root, Dom, Name]),
    case filelib:is_file(F) of
	true ->
	    {reply, {true, F}, Root};
	false ->
	    {reply, false, Root}
    end;

handle_call({list_domain, Dom}, _From, Root) ->
    D = filename:join(Root, Dom),
    case file:list_dir(D) of
	{ok, F} ->
	    FD = lists:map(fun (X) -> filename:join(Dom, X) end, F),
	    {reply, {success, FD}, Root};
	{error, R} ->
	    {reply, {failure, R}, Root}
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


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

open_or_init_ets(R) ->
    F = filename:join([R, "config", "nl.ets"]),
    case filelib:is_regular(F) of
	true ->
	    {ok, Tab} = ets:file2tab(F);
	false ->
	    Tab = ets:new(wrfx_namelists, [set, protected])
    end,
    Tab.
	

move_or_copy(Src, Dst) ->
    case file:rename(Src, Dst) of
	ok ->
	    {success, Dst};
	{error, exdev} ->
	    {ok, _} = file:copy(Src, Dst),
	    ok = file:delete(Src),
	    {success, Dst};
	{error, R} ->
	    {failure, R}
    end.


store_ets_table({Tab, Root}) ->
    Dst = filename:join([Root, "config", "nl.ets"]),
    filelib:ensure_dir(Dst),
    ets:tab2file(Tab, Dst).
