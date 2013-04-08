

Nonterminals namelist sections section key_values key_value values.
Terminals string ',' '&' '/' '='.
Rootsymbol namelist.
Endsymbol '$end'.

namelist -> sections.
sections -> section.
sections -> section sections.

section -> '&' string key_values '/'.
key_values -> key_value.
key_values -> key_value key_values.

key_value -> string '=' values.
values -> string ','.
values -> string ',' values.

