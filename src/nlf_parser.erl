
-module(nlf_parser).
-author(vejmelkam@gmail.com).

-include("include/mcfg.hrl").

-export([parse/1]).


% read in the file line by line and parse the namelists
parse(F) ->
    {ok, D} = file:open(F, [read]),
    Lines = read_content(D, []),
    NL = parse_namelists(Lines, []),
    dict:from_list(NL).


% read the entire file
read_content(D, A) ->
    case io:get_line(D) of
        eof -> file:close(D), A;
        Line -> read_content(D, [reduce(Line)|A])
    end.


reduce(Line) ->
    L2 = string:strip(Line),
    case string:chr(L2, $!) of
        0 ->
            L2;
        _ ->
            [L3|_R] = string:tokens(L2, "!"),
            L3
    end.

parse_namelists(D, Acc) ->
    
    case find_namelist(D) of
        {ok, NLName} ->
            NL = {NLName, parse_namelist(D, dict:new())},
            parse_namelists(D, [NL|Acc]);
        not_found ->
            not_found
    end.

%FIXME: stub
find_namelist(_D) ->
    found.

parse_namelist(D, NLD) ->
    case parse_variable(D) of
        end_of_nl ->
            NLD;
        {ok, V} ->
            parse_namelist(D, [V|NLD])
    end.


parse_variable(D) ->
    {ok, _L} = get_line(D).
            

get_line(D) ->
    {ok, L} = read_line(D),
    case L of
        [] ->
            get_line(D);
        ['!'|_R] ->
            get_line(D);
        _ ->
            {ok, L}
    end.


read_line(D) ->
    case io:get_line(D) of
        eof ->
            parse_error;
        {error, _E} ->
            parse_error;
        L ->
            {ok, string:strip(L)}
    end.


            
