
-module(reg_parser).
-author(vejmelkam@gmail.com).

-include("include/mcfg.hrl").
-export([parse_file/1]).

parse_file(Fname) ->
    {ok, IO} = file:open(Fname, [read]),
    foreach_line(IO, []).


foreach_line(IO, Acc) ->
    case io:get_line(IO, []) of
        eof -> file:close(IO), Acc;
        Line -> Acc2 = parse_reg_line(Line, Acc),
                foreach_line(IO, Acc2)
    end.


parse_reg_line(Line, Acc) ->
    Toks = string:tokens(Line, "\t "),
    case Toks of
        ["rconfig"|R] ->
            parse_rconfig(R,Acc);
        _ ->
            Acc
    end.


parse_rconfig([Type,Name,NLSpec,Numspec|_Rest], Acc) ->
    case string:tokens(NLSpec, ",") of
        ["namelist",NLName] ->
            NS = parse_numspec(Numspec),
            [#nlentry{nlid=NLName, name=Name, type=parse_type(Type), mult=NS}|Acc];
        ["derived"] ->
            Acc
    end.


parse_numspec("max_domains") ->
    max_domains;
parse_numspec("max_moves") ->
    max_moves;
parse_numspec("max_eta") ->
    max_eta;
parse_numspec("max_bogus") ->
    max_bogus;
parse_numspec(Numstr) ->
    {Num, []} = string:to_integer(Numstr),
    Num.
   

parse_type("integer") ->
    integer;
parse_type("real") ->
    real;
parse_type("logical") ->
    logical;
parse_type("character") ->
    char.
