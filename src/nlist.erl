

-module(nlist).
-author("vejmelkam@gmail.com").
-include("include/mcfg.hrl").

-export([entries/1, entry/2, set_entry/3,                % standard namelist access functions
	 to_text/1,                                      % constructs a text representation of the namelist (readable by fortran)
	 convert_values/1]).                             % used by nlfile:parse to autodetect types of values of entries in namelist

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
    S2 = lists:map(fun write_section/1, E),
    lists:flatten(S2).

write_section({N, S}) ->
    E = lists:map(fun write_value_list/1, S),
    ["&", N, "\n", [string:join(E, "\n")|"\n/\n\n"]].

write_value_list({K, V}) ->
    VS = lists:map(fun write_value/1, V),
    [K, "\t=\t", string:join(VS, ",\t"), ","].


write_value(false) ->
    ".false.";
write_value(true) ->
    ".true.";
write_value(X) ->
    io_lib:format("~p", [X]).



%
%  parse tree (see nl_parser.yrl) -> namelist representation transformation code
%  basically autodetects the type of each value and recodes it into an erlang-friendly form
%

convert_values(NL=#nl{entries=E}) ->
    E2 = lists:map(fun ({K,V}) -> {K, convert_type(V)} end, E),
    NL#nl{entries=E2}.


convert_type(L) ->
    convert_type(L, []).
convert_type([], A) ->
    lists:reverse(A);
convert_type([".false."|L], A) ->
    convert_type(L, [false|A]);
convert_type([".true."|L], A) ->
    convert_type(L, [true|A]);
convert_type([X|L], A) ->
    D = lists:foldl(fun (F,Acc) -> try_decode_number(F,X,Acc) end, [], ["~u", "~d", "~f"]),
    case D of
	[] ->
	    convert_type(L, [X|A]);
	[V] ->
	    convert_type(L, [V|A])
    end.


try_decode_number(F,X,[]) ->
    case io_lib:fread(F, X) of
	{ok, [V], []} ->
	    [V];
	{ok, [V], "."} ->   % special case for fortan floats "290." with no trailing zero
	    [V];
	{ok, _V, _Rest} ->
	    [];
	{error, _} ->
	    []
    end;
try_decode_number(_F, _X, A) ->
    A.

