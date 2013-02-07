-module(mcfg_app).

-behaviour(application).
-include("include/mcfg.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    mcfg_sup:start_link().

stop(_State) ->
    ok.
