
%
%  A list of pairs (tuples) that stores a key-value relationship.
%  Preferred in some cases over dictionaries to preserve order
%  of keys (esp. in namelists).
%

-module(plist).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-export([new/0, keys/1, props/1, 
	 setp/3, getp/2, get_list/2, contains/2,
	 update_with/2, update_with/3,
	 store/2]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


new() ->
    [].

keys(P) ->
    [ Key || {Key, _S} <- P ].

props(P) ->
    [ Prop || {_Key, Prop} <- P ].

setp(K, V, []) ->
    [{K,V}];
setp(K, V, [{K,_OldV}|P]) ->
    [{K,V}|P];
setp(K, V, [{K1,V1}|P]) ->
    [{K1,V1}|setp(K, V, P)].


getp(_K, []) ->
    not_found;
getp(K, [{K,V}|_P]) ->
    V;
getp(K, [_E|P]) ->
    getp(K, P).


get_list(K, P) ->
    [ getp(X, P) || X <- K ].


contains(_K, []) ->
    false;
contains(K, [{K,_V}|_P]) ->
    true;
contains(K, [_E|P]) ->
    contains(K, P).


update_with(UP, P) ->
    lists:foldl(fun ({K,V}, Acc) -> setp(K, V, Acc) end, P, UP).

update_with(KL, VL, P) ->
    update_with(lists:zip(KL, VL), P).


store(F, P) ->
    file:write_file(F, P).


-ifdef(TEST).

make_plist_test() ->
    ?assert([] == plist:new()),
    ?assert([{test, 1}] =:= plist:update_with([{test,1}], [])).

-endif.
