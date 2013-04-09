
-module(nl_parser).
-author("vejmelkam@gmail.com").
-include("include/mcfg.hrl").

-export([parse/1]).


parse(T) ->
    NL = parse_namelists(T, dict:new()),
    #nl{id = "undefined", sections=NL}. 

parse_namelists([], NLS) ->
    NLS;
parse_namelists([{'&', _N1},{string,_N2,NLName}|R], NLS) ->
    {R2, NL} = parse_entries(R),
    [{'/', _N3}|R3] = R2,
    parse_namelists(R3, dict:store(NLName, NL, NLS)).

parse_entries(R) ->
    parse_entries(R, dict:new()).

parse_entries(R=[{'/',_N2}|_Rest], C)->
    {R, C};
parse_entries([{string,_N1,Key},{'=',_N2}|R], C) ->
    {V,R2} = parse_values(R),
    parse_entries(R2, dict:store(Key,V,C)).


parse_values(R) ->
    parse_values(R, []).
parse_values([{string, _N1, Val}, {',', _N2}|R], V) ->
    parse_values(R, [Val|V]);
parse_values(R = [{string,_N1,_Key}, {'=', _N2}|_Rest], V) ->
    {lists:reverse(V), R};
parse_values(R = [{'/', _N1}|_Rest], V) ->
    {lists:reverse(V), R};
parse_values([{string,_N1,Val}|R], V) ->
    % if all else fails, try to parse a value with no comma
    % unfortunately it seems the namelist grammar allows this
    parse_values(R, [Val|V]).



    



    
    
    
