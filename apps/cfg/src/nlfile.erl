
-module(nlfile).
-author("vejmelkam@gmail.com").
-include("include/mcfg.hrl").

-export([namelists/1, namelist/2, set_namelist/3,             % namelist manipulation
	 load/1, store/2,                                     % load and store erlang representation of namelist file
	 parse/1, parse/2, to_text/1]).                       % namelist file I/O

load(F) ->
    {ok, C} = file:consult(F),
    C.

store(F, NL) ->
    R = io_lib:format("~p.",[NL]),
    file:write_file(F, R).

namelists(#nlfile{nls=NLS}) ->
    plist:get_keys(NLS).

namelist(Id, #nlfile{nls=NLS}) ->
    plist:get_value(Id, NLS).

set_namelist(NLId, NL, NLF=#nlfile{nls=NLS}) ->
    NLF#nlfile{nls = plist:set_value(NLId, NL, NLS)}.


parse(FName) ->
    parse(FName, FName).

parse(FName, Name) ->
    {ok, T} = nlscanner:scan(FName),
    {ok, NLS} = nlparser:parse(T),
    #nlfile{id=Name, nls=NLS}.
    
to_text(#nlfile{nls=N}) ->
    S2 = lists:map(fun nlist:to_text/1, N),
    lists:flatten(S2).
