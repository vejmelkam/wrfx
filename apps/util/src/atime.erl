

%
%  Atime is a module for date and time arithmetic.
%

-module(atime).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([dt_shift_hours/2, dt_shift_days/2, d_shift_days/2, 
	 dt_hours_since/2,
	 dt_round_hour/2,
	 compare_dt/2, compare_dates/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


dt_round_hour({Date, {H, _M, _S}}, down) ->
    {Date, {H, 0, 0}};
dt_round_hour(DT={_Date, {_H, 0, 0}}, up) ->
    DT;
dt_round_hour(DT, up) ->
    dt_shift_hours(dt_round_hour(DT, down), 1).




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


compare_dates(D1, D2) ->
    compare_tuples_lex(D1, D2).

compare_dt({D1,T1}, {D2, T2}) ->
    case compare_tuples_lex(D1, D2) of
	equal ->
	    compare_tuples_lex(T1, T2);
	X ->
	    X
    end.


compare_tuples_lex({A1, _B1, _C1}, {A2, _B2, _C2}) when A1 > A2 ->
    later;
compare_tuples_lex({A1, _B1, _C1}, {A2, _B2, _C2}) when A1 < A2 ->
    earlier;
compare_tuples_lex({_A1, B1, _C1}, {_A2, B2, _C2}) when B1 > B2 ->
    later;
compare_tuples_lex({_A1, B1, _C1}, {_A2, B2, _C2}) when B1 < B2 ->
    earlier;
compare_tuples_lex({_A1, _B1, C1}, {_A2, _B2, C2}) when C1 > C2 ->
    later;
compare_tuples_lex({_A1, _B1, C1}, {_A2, _B2, C2}) when C1 < C2 ->
    earlier;
compare_tuples_lex(_, _) ->
    equal.



-ifdef(TEST).

dt_hour_shift_test() ->
    ?assert(dt_shift_hours({{2012, 10, 1}, {1, 1, 1}}, 1) =:= {{2012,10,1},{2,1,1}}),
    ?assert(dt_shift_hours({{2012, 10, 1}, {0, 1, 1}}, -1) =:= {{2012,9,30},{23,1,1}}),
    ?assert(dt_shift_hours({{2012, 10, 2}, {0, 1, 1}}, -5) =:= {{2012,10,1},{19,1,1}}).

dt_hours_since_test() ->
    ?assert(dt_hours_since({{2012, 10, 1}, {15, 12, 0}}, {{2012, 10, 1}, {17, 29, 0}}) =:= 2).

-endif.
    
    
