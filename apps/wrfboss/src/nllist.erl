
-module(nllist).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("include/wrf_cfg.hrl").

-export([new/1, namelists/1, namelist/2, set_namelist/3,
	 load/1, store/2,                          % load/store term representation
	 parse/1, parse/2, to_text/1]).            % namelist file I/O

new(Id) ->
    #nllist{id=Id, nls = plist:new()}.


load(F) ->
    {ok, C} = file:consult(F),
    C.

store(F, NL) ->
    R = io_lib:format("~p.",[NL]),
    file:write_file(F, R).

namelists(#nllist{nls=NLS}) ->
    plist:get_keys(NLS).

namelist(Id, #nllist{nls=NLS}) ->
    plist:get_value(Id, NLS).

set_namelist(Id, NL, NLF=#nllist{nls=NLS}) ->
    NLF#nllist{nls = plist:set_value(Id, NL, NLS)}.


parse(FName) ->
    parse(FName, FName).

parse(FName, Name) ->
    {ok, T} = nlscanner:scan(FName),
    {ok, NLS} = nlparser:parse(T),
    #nllist{id=Name, nls=NLS}.
    
to_text(#nllist{nls=N}) ->
    S2 = lists:map(fun nlist:to_text/1, N),
    lists:flatten(S2).
