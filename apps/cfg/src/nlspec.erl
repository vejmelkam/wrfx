
-module(nlspec).
-author("vejmelkam@gmail.com").
-include("include/mcfg.hrl").
-export([id/1, entry_names/1,entries/1, entry/2]).


id(#nlspec{id=I}) ->
    I.

entry_names(#nlspec{entries=E}) ->
    dict:fold(fun (K,_V,A) -> [K|A] end, [], E).

entries(#nlspec{entries=E}) ->
    dict:fold(fun (_K,V,A) -> [V|A] end, [], E).

entry(N, #nlspec{entries=E}) ->
    dict:fetch(N, E).

