

-module(nlfile).
-author("vejmelkam@gmail.com").
-include("include/mcfg.hrl").

-export([sections/1, section/2, set_section/3,             % list sections
	 list_section/2, update_entry/4,                   % section/entry functions
	 load/1, store/2, parse/1, to_text/1, parse/2]).   % I/O of namelists

load(F) ->
    {ok, C} = file:consult(F),
    C.

store(F, NL) ->
    R = io_lib:format("~p.",[NL]),
    file:write_file(F, R).

namelists(#nlfile{nls=NLS}) ->
    plist:get_keys(NLS).

namelist(Id, #nl{nls=NLS}) ->
    plist:find_key(Id, NLS).

set_namelist(NLId, NL, NLF=#nlfile{nls=NLS}) ->
    NLF#nlfile{nls = plist:set_value(NLId, NL, NLS)}.


% parse an existing namelist in a file
parse(FName) ->
    parse(FName, FName).

parse(FName, Name) ->
    {ok, T} = nl_scanner:scan(FName),
    {ok, G} = nl_parser:parse(T),
    #nl{id = Name, sections = tree_to_nlfile(G)}.

tree_to_nlfile(G) ->
    S = nlist:cons_to_list(slist, G),
    lists:map(fun nlist:tree_to_nl/1, S).
    

    
to_text(#nlfile{nls=N}) ->
    S2 = lists:map(fun nlist:to_text/1, E),
    lists:flatten(S2).
