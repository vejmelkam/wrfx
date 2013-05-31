

%% @doc
%% Atime is a module for date and time arithmetic
%% and conversion to string.
%%

-module(atime).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([dt_shift_hours/2, dt_shift_days/2, d_shift_days/2]).
-export([dt_hours_since/2, dt_seconds_between/2]).
-export([dt_round_hours/2]).
-export([is_before/2, is_after/2, compare/2]).
-export([dt_mdhm_str/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


dt_round_hours({Date, {H, _M, _S}}, down) ->
    {Date, {H, 0, 0}};
dt_round_hours(DT={_Date, {_H, 0, 0}}, up) ->
    DT;
dt_round_hours(DT, up) ->
    dt_shift_hours(dt_round_hours(DT, down), 1).


dt_seconds_between(From, To) ->
    ToS = calendar:datetime_to_gregorian_seconds(To),
    FromS = calendar:datetime_to_gregorian_seconds(From),
    ToS - FromS.



dt_shift_hours(DateTime, H) ->
    DTS = calendar:datetime_to_gregorian_seconds(DateTime),
    calendar:gregorian_seconds_to_datetime(DTS + H * 3600).

dt_shift_days({Date, Time}, D) ->
    {d_shift_days(Date, D), Time}.

d_shift_days(Date, D) ->
    DTD = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(DTD + D).

dt_hours_since(DTFrom, DTTo) ->
    DTSTo = calendar:datetime_to_gregorian_seconds(DTTo),
    DTSFrom = calendar:datetime_to_gregorian_seconds(DTFrom),
    (DTSTo - DTSFrom) div 3600.


is_before({{Y1,M1,D1}, {H1,MI1,S1}}, {{Y2,M2,D2}, {H2,MI2,S2}}) ->
    {Y1,M1,D1,H1,MI1,S1} < {Y2,M2,D2,H2,MI2,S2}.

is_after({{Y1,M1,D1}, {H1,MI1,S1}}, {{Y2,M2,D2}, {H2,MI2,S2}}) ->
    {Y1,M1,D1,H1,MI1,S1} > {Y2,M2,D2,H2,MI2,S2}.

compare(DT1, DT2) ->
    case is_before(DT1, DT2) of
	true ->
	    is_before;
	false ->
	    case is_after(DT1, DT2) of
		true ->
		    is_after;
		false ->
		    same
	    end
    end.


dt_mdhm_str({{_Y,M,D}, {H,Min,_S}}) ->
    lists:flatten(io_lib:format("~2..0B-~2..0B_~2..0B:~2..0B", [M, D, H, Min])).


-ifdef(TEST).

dt_hour_shift_test() ->
    ?assert(dt_shift_hours({{2012, 10, 1}, {1, 1, 1}}, 1) =:= {{2012,10,1},{2,1,1}}),
    ?assert(dt_shift_hours({{2012, 10, 1}, {0, 1, 1}}, -1) =:= {{2012,9,30},{23,1,1}}),
    ?assert(dt_shift_hours({{2012, 10, 2}, {0, 1, 1}}, -5) =:= {{2012,10,1},{19,1,1}}).

dt_hours_since_test() ->
    ?assert(dt_hours_since({{2012, 10, 1}, {15, 12, 0}}, {{2012, 10, 1}, {17, 29, 0}}) =:= 2).

-endif.
    
    
