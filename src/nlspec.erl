
-module(nlspec).
-author(vejmelkam@gmail.com).

-export([entry_names/1,entries/1]).


entry_names(NLS) ->
    dict:fold(fun (K,_V,A) -> [K|A] end, [], NLS).

entries(NLS) ->
    dict:fold(fun (_K,V,A) -> [V|A] end, [], NLS).

