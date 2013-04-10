
Nonterminals namelist sections section entries entry values.
Terminals key string '&' '/'.
Rootsymbol namelist.
Endsymbol '$end'.

namelist -> sections : '$1'.
sections -> section : '$1'.
sections -> section sections : {slist, '$1', '$2'}.

section -> '&' string '/' : {section, element(3, '$2'), empty}.
section -> '&' string entries '/' : {section, element(3, '$2'), cons_to_list('$3')}.
entries -> entry : '$1'.
entries -> entry entries : {entries, '$1', '$2'}.

entry -> key values : {element(3,'$1'), cons_to_list(values, '$2')}.
values -> string : element(3, '$1').
values -> string values : {values, element(3, '$1'), '$2'}.

Erlang code.

% This function is also used by nlfile and only there, we keep it here for now.
cons_to_list(T, L) ->
    cons_to_list(T, L, []).
cons_to_list(T, {T, E, R}, A) ->
    cons_to_list(T, R, [E|A]);
cons_to_list(_T, E, A) ->
    lists:reverse([E|A]).

