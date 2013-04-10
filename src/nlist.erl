

-module(nlist).
-author("vejmelkam@gmail.com").
-include("include/mcfg.hrl").

-export([sections/1, section/2, set_section/3,             % list sections
	 list_section/2, update_entry/4,                   % section/entry functions
	 load/1, store/2, parse/1, to_text/1, parse/2]).    % I/O of namelists

load(F) ->
    {ok, C} = file:consult(F),
    C.

store(F, NL) ->
    R = io_lib:format("~p.",[NL]),
    file:write_file(F, R).

sections(#nl{sections=Sections}) ->
    plist:get_keys(Sections).

section(SName, #nl{sections=Sections}) ->
    plist:find_key(SName, Sections).

set_section(SName, Sec, NL=#nl{sections=Sections}) ->
    NL#nl{sections = plist:set_value(SName, Sec, Sections)}.


% Update a value related to the given section and key
update_entry(Sec, Key, Val, NL) ->
    S = section(Sec, NL),
    S2 = plist:set_value(Key, Val, S),
    set_section(Sec, S2, NL).


% Return the keys in given section
list_section(SName, NL) ->
    plist:get_keys(section(SName, NL)).


% parse an existing namelist in a file
parse(FName) ->
    parse(FName, FName).

parse(FName, Name) ->
    {ok, T} = nl_scanner:scan(FName),
    {ok, G} = nl_parser:parse(T),
    #nl{id = Name, sections = tree_to_sections(G)}.

tree_to_sections(G) ->
    S = cons2list(slist, G),
    lists:map(fun decode_section/1, S).
    

decode_section({section, SName, empty}) ->
    {SName, []};
decode_section({section, SName, Entries}) ->
    S = cons2list(entries, Entries),
    L = lists:map(fun ({K, V}) -> {K, convert_type(cons2list(values, V))} end, S),
    {SName, L}.


cons2list(T, L) ->
    cons2list(T, L, []).
cons2list(T, {T, E, R}, A) ->
    cons2list(T, R, [E|A]);
cons2list(_T, E, A) ->
    lists:reverse([E|A]).


convert_type(L) ->
    convert_type(L, []).
convert_type([], A) ->
    lists:reverse(A);
convert_type([".false."|L], A) ->
    convert_type(L, [false|A]);
convert_type([".true."|L], A) ->
    convert_type(L, [true|A]);
convert_type([X|L], A) ->
    D = lists:foldl(fun (F,Acc) -> try_decode(F,X,Acc) end, [], ["~u", "~d", "~f"]),
    case D of
	[] ->
	    convert_type(L, [X|A]);
	[V] ->
	    convert_type(L, [V|A])
    end.


try_decode(F,X,[]) ->
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
try_decode(_F, _X, A) ->
    A.

    

to_text(#nl{sections=SS}) ->
    S2 = lists:map(fun write_section/1, SS),
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

