
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
values -> string : [element(3, '$1')].
values -> string values : [element(3, '$1')|'$2'].

Erlang code.

-include("include/mcfg.hrl").

