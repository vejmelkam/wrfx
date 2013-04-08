
-module(nl_parser).
-author("vejmelkam@gmail.com").

-include("include/mcfg.hrl").

-export([parse/1]).


% read in the file line by line (removing comments)
% parse 
parse(F) ->
    {ok, D} = file:open(F, [read]),
    Lines = read_content(D, []),
    NL = parse_namelists(Lines, []),
    dict:from_list(NL).


% read the entire file
read_content(D, A) ->
    case io:get_line(D) of
        eof -> 
	    file:close(D),
	    A2 = lists:filter(fun(X) -> length(X) > 0 end, A),
	    lists:reverse(A2);
        {ok, Line} -> 
	    read_content(D, [reduce(Line)|A])
    end.


% strip whitespace and comments at end of line
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


find_namelist(D) ->
    case get_line(D) of
	[$&|NLName] ->
	    {ok, NLName};
	[] ->
	    find_namelist(D);
	eof ->
	    not_found
end.


parse_namelist(D, NLD) ->
    case parse_variable(D) of
        end_of_nl ->
            NLD;
        {ok, V} ->
            parse_namelist(D, [V|NLD])
    end.


parse_variable(_D) ->
    ok.
            

get_line(D) ->
    Data = io:get_line(D),
    case Data of
	% end of file
        eof ->
	    eof;
	% comment line (skip)
	{ok, L} ->
            check_line(string:strip(L), D)
    end.


check_line([], D) ->
    get_line(D);
check_line([$!|_R], D) ->
    get_line(D);
check_line(L, D) ->
    L.

