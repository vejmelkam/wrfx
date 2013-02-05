
-module(nlist).
-author(vejmelkam@gmail.com).

-export([entries/1, names/1]).

% List all entry names in the current namelist (dict).
% 
entries(D) ->
    dict:fold(fun (_N, E, A) -> [E|A] end, [], D).


names(D) ->
    dict:fold(fun (N, _E, A) -> [N|A] end, [], D).


