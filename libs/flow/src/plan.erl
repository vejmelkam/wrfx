

%
%
%  Simple module for plan management.
%


-module(plan).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include_lib("flow/include/flow.hrl").
-export([count_steps/1]).

count_steps(#plan{tasks=T}) ->
    length(T).
