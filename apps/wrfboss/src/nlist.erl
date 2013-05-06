

-module(nlist).
-author("vejmelkam@gmail.com").
-include("include/wrf_cfg.hrl").

-export([all_entries/1,
	 entries/2, entry/2, set_entry/3,
	 to_text/1]).  % renders namelist to FORTRAN readable format

% Return the keys in given section
all_entries(#nl{entries=E}) ->
    plist:keys(E).

entry(K, #nl{entries=E}) ->
    plist:getp(K, E).

set_entry(K, V, NL=#nl{entries=E}) ->
    NL#nl{entries=plist:setp(K, V, E)}.

entries(K, #nl{entries=E}) ->
    lists:flatten(plist:get_list(K, E)).



%
%  namelist -> text rendering code
%

to_text(#nl{id=Id, entries=E}) ->
    S2 = lists:map(fun write_entry/1, E),
    lists:flatten(["&", Id, $\n, S2, "/\n\n"]).


write_entry({K, V}) ->
    VS = lists:map(fun write_value/1, V),
    [K, lists:duplicate(36-length(K), $ ), "=\t", string:join(VS, ",\t"), ", \n"].


write_value(false) ->
    ".false.";
write_value(true) ->
    ".true.";
write_value(X) when is_list(X) ->
    io_lib:format("'~s'", [X]);
write_value(X) ->
    io_lib:format("~p", [X]).
