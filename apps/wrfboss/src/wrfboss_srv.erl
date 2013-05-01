
%
%  The main process in the application.  Accepts job requests,
%  starts/monitors/cancels jobs, stores/serves results in various forms.
%
%

-module(wrfboss_srv).
-behaviour(gen_server).

-export([start_link/0,init/1,terminate/2,code_change/3]).
-export([handle_call/3,handle_cast/2,handle_info/2]).
-export([start/0, stop/0, echo/1]).

start_link() ->

    % ensure mnesia is up & init tables if needed
    mnesia:start(),
    init_mnesia_tables(),

    % start this server
    gen_server:start_link({local, mcfg_srv}, mcfg_srv, [], []).


init(_A) ->
    erlang:process_flag(trap_exit, true),
    {ok, none}.

handle_call({echo, X}, _F, B) ->
    {reply, X, B};

handle_call(shutdown, _F, B) ->
    {stop, shutdown, stopped, B};

handle_call(A, F, B) ->
    io:format("mcfg_srv don't know how to handle [~p] from [~p].~n", [A, F]),
    {reply, A, B}.

handle_cast(A, B) ->
    io:format("mcfg_srv received cast [~p] in state [~p].~n", [A, B]),
    {noreply, B}.

handle_info(I, S) ->
    io:format("received handle_info call [~p] in state [~p].~n", [I, S]),
    {noreply, S}.

% no provisions for code change in this version
code_change(_OldVsn, S, _Ex) ->
    {ok, S}.

terminate(T, S) ->
    io:format("mcfg_srv: terminating with reason ~p, (state [~p]).~n", [T, S]),
    ok.

%% private functions

init_mnesia_tables() ->
    % initialize a mnesia table if it does not exist
    case lists:member(mcfg, mnesia:system_info(tables)) of
        false ->
            mnesia:create_table(mcfg_spec, [{record_name, mcfg_spec}])
    end.


echo(X) ->
    gen_server:call(mcfg_srv, {echo, X}).

stop() ->
    application:stop(mcfg).

start() ->
    application:start(mcfg).

