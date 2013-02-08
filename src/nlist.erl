
-module(nlist).
-author(vejmelkam@gmail.com).

-export([entries/1, names/1,get_entry/2]).

% List of entries in the namelist.
% 
entries(D) ->
    dict:fold(fun (_N, E, A) -> [E|A] end, [], D).


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

        
