-module(xcfg).
-author(vejmelkam@gmail.com).

-include("include/mcfg.hrl").

-export([new/1, new/2, load/1, id/1, vars/1, set/3, get/2,
	 to_string/1, from_string/1,set_many/2]).


new(ID) ->
    #cfg_chunk{id=ID, kvstore=dict:new()}.


new(ID, Props) ->
    C = new(ID),
    set_many(Props, C).


id(#cfg_chunk{id=ID}) ->
    ID.


vars(#cfg_chunk{kvstore=D}) ->
    dict:fold(fun (K,_V,A) -> [K|A] end, [], D).


set_many(Prop, C=#cfg_chunk{kvstore=D}) ->
    D2 = lists:foldl(fun ({K,V}, CD) -> dict:store(K, V, CD) end, D, Prop),
    C#cfg_chunk{kvstore=D2}.


set(Var,Val, C=#cfg_chunk{kvstore=D}) ->
    C#cfg_chunk{kvstore=dict:store(Var,Val,D)}.


get(K, #cfg_chunk{kvstore=D}) ->
    dict:fetch(K, D).
  

to_string(C=#cfg_chunk{kvstore=D}) ->
    L = dict:to_list(D),
    R = io_lib:format("~p.",[C#cfg_chunk{kvstore=L}]),
    lists:flatten(R).


from_string(S) ->
    {ok,Tokens,_} = erl_scan:string(S),
    {ok,C = #cfg_chunk{kvstore=L}} = erl_parse:parse_term(Tokens),
    D = dict:from_list(L),
    C#cfg_chunk{kvstore=D}.


load(F) ->
    {ok, B} = file:read_file(F),
    S = binary:bin_to_list(B),
    from_string(S).
