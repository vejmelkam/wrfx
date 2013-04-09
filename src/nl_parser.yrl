

Nonterminals namelists namelist entries entry values.
Terminals string ',' '&' '/' '=' ';'.
Rootsymbol namelists.
Endsymbol '$end'.

namelists -> namelist.
namelists -> namelist namelists.

namelist -> '&' string entries '/'.

entries -> entry.
entries -> entry entries.

entry -> string '=' values.
values -> string ';'.
values -> string ',' values ';'.


