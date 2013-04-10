
Nonterminals namelist sections section entries entry values.
Terminals key string '&' '/'.
Rootsymbol namelist.
Endsymbol '$end'.

namelist -> sections : '$1'.
sections -> section : ['$1'].
sections -> section sections : ['$1'|'$2'].

section -> '&' string '/' : {element(3,'$2'), #nl{id=element(3, '$2'), entries=[]}}.
section -> '&' string entries '/' : {element(3,'$2'), #nl{id=element(3, '$2'), entries='$3'}}.
entries -> entry : ['$1'].
entries -> entry entries : ['$1'|'$2'].

entry -> key values : {element(3,'$1'), '$2'}.
values -> string : [read_type(element(3, '$1'))].
values -> string values : [read_type(element(3, '$1'))|'$2'].

Erlang code.

-include("include/mcfg.hrl").

read_type(".false.") ->
    false;
read_type(".true.") ->
    true;
read_type(X) ->
    D = lists:foldl(fun (F,Acc) -> try_decode_number(F,X,Acc) end, [], ["~u", "~d", "~f"]),
    case D of
	[] ->
	    X;
	[V] ->
	    V
    end.


try_decode_number(F,X,[]) ->
    case io_lib:fread(F, X) of
	{ok, [V], []} ->
	    [V];
	{ok, [V], "."} ->   % special case for fortan floats "290." with no trailing zero
	    [V];
	{ok, _V, _Rest} ->
	    [];
	{error, _} ->
	    []
    end;
try_decode_number(_F, _X, A) ->
    A.

