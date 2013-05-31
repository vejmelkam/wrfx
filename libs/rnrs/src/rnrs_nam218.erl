
%% @doc
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
-export([domain/0, url_prefix/0, manifest/3, vtable/0]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @spec vtable() -> string()
vtable() ->
    "ungrib/Variable_Tables/Vtable.NAM".


%% @spec domain() -> string()
domain() ->
    "nam_218".

%% @spec url_prefix() -> string()
url_prefix() ->
    "http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod/".

%% @spec manifest(From :: datetime(), To :: datetime(), Delta :: integer()) -> [string()]
manifest(From, To, Delta) ->

    % estimate the latest cycle that was run and is available online
    UTCNow_3 = atime:dt_shift_hours(calendar:universal_time(), -3),
    LatestCycle = get_cycle_time(UTCNow_3, Delta),

    % adjust from and to to be aligned on the hour (first adjustment step)
    FromAdj = atime:dt_round_hours(From, down),
    ToAdj = atime:dt_round_hours(To, up),

    % retrieve NAM 218 file names covering all times between From and To (inclusive)
    % NAM files are hourly up to 36 hrs, then available every 3 hours until 84
    EmptyCoverage = {atime:dt_shift_hours(ToAdj, 1), atime:dt_shift_hours(FromAdj, -1)},

    try
	build_file_list(FromAdj, ToAdj, LatestCycle, EmptyCoverage, no_ref, [])
    catch
	invalid_forecast_hour ->
	    {error, io_lib:format("unavailable data for time range ~s to ~s with delta ~p~n",
				  [esmf:time_to_string(From), esmf:time_to_string(To), Delta])}
    end.


build_file_list(Now, To, LatestCycle, Coverage={_CFrom, CTo}, LastRef, Files) when To > CTo->
    NextNow = atime:dt_shift_hours(Now, 1),
    CT = cull_cycle(get_cycle_time(Now, 0), LatestCycle),
    H = forecast_hour(atime:dt_hours_between(CT, Now)),
    case {CT, H} == LastRef of
	true ->
	    build_file_list(NextNow, To, LatestCycle, Coverage, LastRef, Files);
	false ->
	    RefTime = atime:dt_shift_hours(CT, H),
	    File = construct_file_ref(CT, H),
	    build_file_list(NextNow, To, LatestCycle, update_coverage(Coverage, RefTime), {CT, H}, [File|Files])
    end;

build_file_list(_Now, _To, _LatestCycle, Coverage, _LastRef, Files) ->
    {ok, Coverage, Files}.


update_coverage({A, B}, RefTime) when RefTime < A ->
    update_coverage({RefTime, B}, RefTime);
update_coverage({A, B}, RefTime) when RefTime > B->
    update_coverage({A, RefTime}, RefTime);
update_coverage(Cov, _RefTime) ->
    Cov.


forecast_hour(H) when (0 =< H) and (H < 37) ->
    H;
forecast_hour(H) when (36 < H) and (H < 85) ->
    ((H - 36) div 3) * 3 + 36;
forecast_hour(_H) ->
    throw(invalid_forecast_hour).



construct_file_ref({{Y,M,D}, {CHr, 0, 0}}, H) ->
    lists:flatten(io_lib:format("nam.~4..0B~2..0B~2..0B/nam.t~2..0Bz.awphys~2..0B.grb2.tm00", [Y, M, D, CHr, H])).

cull_cycle(C1, C2) ->
    case atime:is_before(C1, C2) of
	true ->
	    C1;
	false ->
	    C2
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
get_cycle_time({Date, _Time}, 0) ->
    {Date, {0, 0, 0}};
get_cycle_time(DateTime, Delta) ->
    atime:dt_shift_hours(get_cycle_time(DateTime, Delta - 1), -6).





-ifdef(TEST).

cycle_time_test() ->
    ?assert(get_cycle_time({{2013, 5, 1}, {1, 30, 15}}, 0) =:= {{2013, 5, 1}, {0, 0, 0}}),
    ?assert(get_cycle_time({{2013, 5, 1}, {1, 30, 15}}, 1) =:= {{2013, 4, 30}, {18, 0, 0}}).
    
-endif.
