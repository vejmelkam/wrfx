

-module(mcfg_srv).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2]).

start_link() ->

    init_mnesia_tables(),

    gen_server:start_link({local, mcfg_srv}, mcfg_srv, [], []).


init(_A) ->
    {ok, none}.

handle_call(A, _F, B) ->
    {reply, A, B}.

handle_cast(A, B) ->
    {noreply, B}.


%% private functions

init_mnesia_tables() ->
    % initialize a mnesia table if it does not exist
    case lists:member(mcfg, mnesia:system_info(tables)) of
        false ->
            mnesia:create_table(mcfg_spec, [{record_name, mcfg_spec}])
    end.





