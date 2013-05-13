
%
%  A resolver computes storage ids and URLs for GRIB files.
%  Due to operational constraints, it may happen that current
%  cycle run files are unavailable, in this case, older cycle
%  run files may be requested using CycleRunDelta > 0 parameter.
%
%  This resolver computes a manifest for the NAM grid #218
%  84-hr forecast source.
%

-module(rnrs_nam218).
-author("Martin Vejmelka <vejmelkam@gmail.com").
-export([storage_prefix/0, url_prefix/0, manifest/3]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


storage_prefix() ->
    "nam_218".


url_prefix() ->
    "http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod/".


manifest(From = {{_, _, _}, {_, 0, 0}}, To = {{_, _, _}, {_, 0, 0}}, Delta) ->

    % estimate the latest cycle that was run and is available online
    UTCNow_3 = atime:shift_hours(calendar:universal_time(), -3),
    LatestCycle = get_cycle_time(UTCNow_3, Delta),



    % retrieve NAM 218 file names covering all times between From and To (inclusive)
    % NAM files are hourly up to 36 hrs, then available every 3 hours until 84
    build_file_list(From, To, LatestCycle, []).



build_file_list(From, To, LatestCycle, Files) ->
    case atime:compare_dates(From, To) of
	later ->
	    Files;
	_ ->
	    {File, Next} = construct_file_name(From, LatestCycle),
	    build_file_list(Next, To, LatestCycle, [File|Files])
    end.


construct_file_ref(DT, LatestCycle) ->
    {D, CT} = cull_cycle(get_cycle_time(DT, 0), LatestCycle),
    Hrs = atime:hours_since(DT, {D, {CT, 0, 0}}),
    {construct_file_ref(D, CT, Hrs), next_time(DT, Hrs)}.


construct_file_ref(_D, _CT, Hr) when Hr > 84 ->
    no_such_file;
construct_file_ref({Y,M,D}, Ct, Hr) when Hr < 37 ->
    io_lib:format("~4..0B~2..0B~2..0B/nam.t~2..0Bz.awphys~2..0B.grb2.tm00", [Y, M, D, Ct, Hr]);
construct_file_ref({Y,M,D}, Ct, Hr) ->
    io_lib:format("~4..0B~2..0B~2..0B/nam.t~2..0Bz.awphys~2..0B.grb2.tm00", [Y, M, D, Ct, ((Hr - 36) div 3 * 3) + 36 ]).


next_time(DT, Hrs) when Hrs < 36 ->
    atime:dt_shift_hours(DT, 1);
next_time(DT, Hrs) ->
    atime:dt_shift_hours(DT, 3).
    


cull_cycle(C1, C2) ->
    case atime:compare_dt(C1, C2) of
	later ->
	    C2;
	_ ->
	    C1
    end.

    

%
%  The function get_cycle_time retrieves a description of the cycle,
%  which is closest to the supplied datetime if Delta is zero.
%  Otherwise it returns the cycle that is Delta cycles before the
%  closest cycle.
%
get_cycle_time({Date, {H, _M, _S}}, 0) when H >= 18->
    {Date, {18, 0, 0}};
get_cycle_time({Date, {H, _M, _S}}, 0) when H >= 12->
    {Date, {12, 0, 0}};
get_cycle_time({Date, {H, _M, _S}}, 0) when H >= 6->
    {Date, {6, 0, 0}};
get_cycle_time({Date, {H, _M, _S}}, 0) ->
    {Date, {0, 0, 0}};
get_cycle_time(DateTime, Delta) ->
    atime:dt_shift_hours(get_cycle_time(DateTime, Delta-1), -6).





-ifdef(TEST).

construct_name_test() ->
    ?assert(construct_file_ref(a, b, 85) =:= no_such_file),
    ?assert(construct_file_ref({2012, 5, 4}, 18, 10) =:= "20120504/nam.t18z.awphys10.grb2.tm00"),
    ?assert(construct_file_ref({2012, 5, 4}, 18, 41) =:= "20120504/nam.t18z.awphys39.grb2.tm00").
