

-module(wrfx_stor).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-behavior(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([file_resolve/1, file_exists/1, file_remove/1, file_store/2]).  % file API
-export([namelist_store/2, namelist_retrieve/1, namelist_all/0]).       % nl API
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
file_store(ID, File) ->
    gen_server:call(?SERVER, {file_store, ID, File}).

%% @spec file_resolve(ID::id()) -> string()
file_resolve(ID) -> 
    gen_server:call(?SERVER, {file_resolve, ID}).

%% @spec file_exists(ID::id()) -> {true, F} | false
file_exists(ID) ->
    gen_server:call(?SERVER, {file_exists, ID}).

%% @spec file_remove(ID::id()) -> success | {failure, R}
file_remove(ID) ->
    gen_server:call(?SERVER, {file_remove, ID}).

stop_storage() ->
    gen_server:call(?SERVER, terminate).


namelist_all() ->
    gen_server:call(?SERVER, nl_list).

namelist_store(ID, NL) ->
    gen_server:call(?SERVER, {nl_store, ID, NL}).

namelist_retrieve(ID) ->
    gen_server:call(?SERVER, {nl_retrieve, ID}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, Root} = wrfx_cfg:get_conf(storage_root),
    Tab = open_or_init_ets(Root),
    ok = filelib:ensure_dir(filename:join(Root, "touch")),
    {ok, {Tab, Root}}.


handle_call({file_store, {Dom, Name}, F}, _From, S={_Tab, Root}) ->
    Dst = filename:join([Root, Dom, Name]),
    case filelib:ensure_dir(Dst) of
	ok ->
	    R = move_or_copy(F, Dst),
	    {reply, R, S};
	{error, E} ->
	    {reply, {failure, E}, S}
    end;

handle_call({file_remove, {Dom, Name}}, _From, S={_Tab, Root}) ->
    F = filename:join([Root, Dom, Name]),
    case file:delete(F) of
	ok ->
	    {reply, success, S};
	{error, R} ->
	    {reply, {failure, R}, S}
    end;

handle_call({file_resolve, {Dom, Name}}, _From, S={_Tab, Root}) ->
    F = filename:join([Root, Dom, Name]),
    {reply, F, S};

handle_call({file_exists, {Dom, Name}}, _From, S={_Tab,Root}) ->
    F = filename:join([Root, Dom, Name]),
    case filelib:is_file(F) of
	true ->
	    {reply, {true, F}, S};
	false ->
	    {reply, false, S}
    end;

handle_call({nl_store, ID, NL}, _From, S={Tab, _R}) ->
    ets:insert(Tab, {ID, NL}),
    store_ets_table(S),
    {reply, success, S};

handle_call({nl_retrieve, ID}, _From, S={Tab, _R}) ->
    case ets:lookup(Tab, ID) of
	[{ID, NL}] ->
	    {reply, {ok, NL}, S};
	_ ->
	    {reply, not_found, S}
    end;

handle_call(nl_list, _From, S={Tab, _R}) ->
    Lst = ets:foldl(fun ({ID, _NL}, A) -> [ID|A] end, [], Tab),
    {reply, Lst, S};

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
