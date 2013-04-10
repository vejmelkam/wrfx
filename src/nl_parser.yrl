
Nonterminals namelist sections section entries entry values.
Terminals key string '&' '/'.
Rootsymbol namelist.
Endsymbol '$end'.

namelist -> sections : '$1'.
sections -> section : '$1'.
sections -> section sections : {slist, '$1', '$2'}.

section -> '&' string '/' : {section, element(2, '$2'), empty}.
section -> '&' string entries '/' : {section, element(2, '$2'), '$3'}.
entries -> entry : '$1'.
entries -> entry entries : {entries, '$1', '$2'}.

entry -> key values : {entry, element(2,'$1'), '$2'}.
values -> string : element(2, '$1').
values -> string values : {values, element(2, '$1'), '$2'}.

