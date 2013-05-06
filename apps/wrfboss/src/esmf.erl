
%
%  ESMF time module: read/write ESMF time.
%
%

-module(esmf).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-export([parse_time/1, time_to_string/1]).


parse_time([Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2,$_,H1,H2,$:,MIN1,MIN2,$:,S1,S2]) ->
    {{list_to_integer([Y1,Y2,Y3,Y4]), list_to_integer([M1,M2]), list_to_integer([D1,D2])},
     {list_to_integer([H1,H2]), list_to_integer([MIN1,MIN2]), list_to_integer([S1,S2])}}.


time_to_string({{Y,Mo,D}, {H,Min,S}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B_~2..0B:~2..0B:~2..0B", [Y,Mo,D,H,Min,S]).
