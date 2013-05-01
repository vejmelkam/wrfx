

-module(nlist).
-author("vejmelkam@gmail.com").
-include("include/wrf_cfg.hrl").

-export([entries/1, entry/2, set_entry/3,
	 to_text/1]).  % renders namelist to FORTRAN readable format

% Return the keys in given section
entries(#nl{entries=E}) ->
    plist:get_keys(E).

entry(K, #nl{entries=E}) ->
    plist:get_value(K, E).

set_entry(K, V, NL=#nl{entries=E}) ->
    NL#nl{entries=plist:set_value(K, V, E)}.



%
%  namelist -> text rendering code
%

to_text(#nl{entries=E}) ->
    S2 = lists:map(fun write_entry/1, E),
    lists:flatten(S2).

write_entry({K, V}) ->
    VS = lists:map(fun write_value/1, V),
    [K, "\t=\t", string:join(VS, ",\t"), ","].


write_value(false) ->
    ".false.";
write_value(true) ->
    ".true.";
write_value(X) ->
    io_lib:format("~p", [X]).
