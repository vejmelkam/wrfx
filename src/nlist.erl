
-module(nlist).
-author("vejmelkam@gmail.com").
-include("include/mcfg.hrl").

-export([sections/1, section/2, set_section/3, list_section/2, update/4, load/1, store/2]).

load(F) ->
    {ok, C} = file:consult(F).


store(F, NL) ->
    R = io_lib:format("~p.",[NL]),
    file:write_file(F, R).


% List of sections in the namelist.
% 
sections(#nl{sections=Sections}) ->
    dict:fetch_keys(Sections).

% Retrieve the section dictionary by name of the sections.
%
section(SName, #nl{sections=Sections}) ->
    dict:fetch(SName, Sections).

set_section(SName, SDict, NL=#nl{sections=Sections}) ->
    NL#nl{sections=dict:store(SName, SDict, Sections)}.


% Update a value related to the given section and key
update(Sec, Key, Val, NL=#nl{sections=Sections}) ->
    S = section(Sec, NL),
    S2 = dict:store(Key, Val, S),
    set_section(Sec, S2, NL).


% Return the keys in given section
list_section(SName, NL=#nl{sections=Sections}) ->
    S = section(Sections, NL),
    dict:fetch_keys(S).
