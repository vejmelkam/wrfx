-module(mcfg_app).

-behaviour(application).
-include("include/mcfg.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    % initialize a mnesia table
    case lists:member(mcfg, mnesia:system_info(tables)) of
        false ->
            mnesia:create_table(mcfg_spec, [{record_name, mcfg_spec}])
    end,

    mcfg_sup:start_link().

stop(_State) ->
    ok.
