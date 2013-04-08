%
%  This module renders the namelist object into a string.
%

-module(nl_writer).
-author("vejmelkam@gmail.com").
-include("include/mcfg.hrl").
-export([write/1]).

write(#nl{sections=SS}) ->
    S2 = lists:map(fun write_section/1, SS),
    lists:flatten(S2).

write_section({N, S}) ->
    lists:reverse(["/" | dict:fold(fun write_value/3, [N], S)]).

write_value(K, V, A) when is_list(V) ->
    VS = lists:map(fun (X) -> io_lib:format("~p", [X]) end, V),
    [[K, "~t=~t"] ++ string:join(VS, ",~t")|A];
write_value(K, V, A) ->
    [[K, "~t=~t", io_lib:format("~p", [V])]|A].

    
    


