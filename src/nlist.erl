
-module(nlist).
-author(vejmelkam@gmail.com).

-export([entries/1, names/1, get_entry/2, load/1, store/2]).

load(F) ->
    {ok, C} = file:consult(F).


store(F, NL) ->
    R = io_lib:format("~p.",[NL]),
    file:write_file(F, R).


% List of entries in the namelist.
% 
nlists(#nlists{id=NLS}) ->
    lists:foldl(fun (#cfg_chunk{id=E}, A) -> [E|A] end, [], NLS).


% List all entry names in the current namelist (dict). 
%
names(D) ->
    dict:fold(fun (N, _E, A) -> [N|A] end, [], D).


% Retrieve an entry by name.
%
get_entry(N, D) ->
    case dict:is_key(N, D) of
        true ->
            dict:fetch(N, D);
        false ->
            no_such_entry
    end.
