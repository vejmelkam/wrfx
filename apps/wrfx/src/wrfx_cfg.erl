

%% @doc
%% The configuration server for wrfx.  The configuration is stored as
%% a sequence of erlang terms, so they can b read in using {@link file:consult/1}.
%% On first startup, if the file does not exist, some defaults are given.
%%


-module(wrfx_cfg).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-behavior(gen_server).
-define(SERVER, ?MODULE).
-define(CFG_FILE, ".wrfxcfg").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([get_conf/1]).   % cfg API

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


%% @spec get_conf(Key::string()) -> term()
get_conf(Key) ->
    gen_server:call(?SERVER, {get_conf, Key}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------


init(_Args) ->
    {ok, [[H]]} = init:get_argument(home),
    C = filename:join(H, ?CFG_FILE),
    {ok, init_state(C)}.


handle_call({get_conf, Key}, _From, List) ->
    case plist:contains(Key, List) of
	{true, V} ->
	    {reply, {ok, V}, List};
	false ->
	    {reply, {error, not_found}, List}
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


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


init_state(C) ->
    case filelib:is_file(C) of
	true ->
	    load_terms(C);
	false ->
	    S = default_state(),
	    plist:store(C, S),
	    S
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
    [ {storage_root, filename:join(H, "wrfx_stor")},
      {workspace_root, filename:join(H, "wrfx_workspace")} ].
