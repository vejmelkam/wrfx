-module(wrfx_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % ensure the database backend is ready
    wrfx_db:start(),

    % start application supervisor
    wrfx_sup:start_link().

stop(_State) ->
    ok.
