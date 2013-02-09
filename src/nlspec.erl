
-module(nlspec).
-author(vejmelkam@gmail.com).

-export([entry_names/1,entries/1, entry/2]).


entry_names(NLS) ->
    dict:fold(fun (K,_V,A) -> [K|A] end, [], NLS).

entries(NLS) ->
    dict:fold(fun (_K,V,A) -> [V|A] end, [], NLS).

entry(N, NLS) ->
    dict:fetch(N, NLS).

