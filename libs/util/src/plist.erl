
%
%  A list of pairs (tuples) that stores a key-value relationship.
%  Preferred in some cases over dictionaries to preserve order
%  of keys (esp. in namelists).
%

-module(plist).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-export([new/0, keys/1, props/1, 
	 setp/3, getp/2, getp/3, get_list/2, contains/2, contains_keys/2,
	 update_with/2, update_with/3,
	 store/2, load/1]).


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


% Requesting a key that's not in the plist is an error
% that needs to be fixed in the caller
getp(K, []) ->
    throw({not_found, K});
getp(K, [{K,V}|_P]) ->
    V;
getp(K, [_E|P]) ->
    getp(K, P).


getp(_K, [], Def) ->
    Def;
getp(K, [{K,V}|_Rest], _Def) ->
    V;
getp(K, [_NoMatch|Rest], Def) ->
    getp(K, Rest, Def).


get_list(Ks, P) ->
    [ getp(K, P) || K <- Ks ].


contains(_K, []) ->
    false;
contains(K, [{K,V}|_P]) ->
    {true, V};
contains(K, [_E|P]) ->
    contains(K, P).

contains_keys(Ks, P) ->
    contains_keys(Ks, P, []).
contains_keys([], _P, []) ->
    true;
contains_keys([], _P, Missing) ->
    {false, Missing};
contains_keys([K|Ks], P, Missing) ->
    case contains(K, P) of
	true ->
	    contains_keys(Ks, P, Missing);
	false ->
	    contains_keys(Ks, P, [K|Missing])
    end.


update_with(UP, P) ->
    lists:foldl(fun ({K,V}, Acc) -> setp(K, V, Acc) end, P, UP).

update_with(KL, VL, P) ->
    update_with(lists:zip(KL, VL), P).


store(F, P) ->
    {ok, D} = file:open(F, [write]),
    lists:map(fun (X) -> file:write(D, io_lib:fwrite("~p.~n", [X])) end, P),
    file:close(D).

load(F) ->
    {ok, C} = file:consult(F),
    C.


-ifdef(TEST).

make_plist_test() ->
    ?assert([] == plist:new()),
    ?assert([{test, 1}] =:= plist:update_with([{test,1}], [])).

-endif.
