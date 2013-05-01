
%
%  This module tranforms specific configuration key/value pairs
%  in the wrf_cfg record and a given namelist into
%  a namelist with modifications required by the config.
%
%  Also, the module reads in information from a namelist into
%  a configuration object that summarizes relevant parts of the namelist.
%

-module(wrf_cfg).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("include/wrf_cfg.hrl").
-export([write_time_range/2, write_io_policy/2]).


write_time_range(#wrf_cfg{cfg=C, wrf_spec=MS}, NL) ->

    % both of these must be standard erlang dates
    Ts={{Sy, Sm, Sd}, {Shr, Smin, Ssec}} = dict:fetch(dt_from, C),
    Te={{Ey, Em, Ed}, {Ehr, Emin, Esec}} = dict:fetch(dt_to, C),
    {Rdays, {Rhrs, Rmins, Rsecs}} = calendar:time_difference(Te, Ts),

    TCSpec = mcfg_spec:nlspec("time_control", MS),

    update_nl_with_list([{"run_days", Rdays},
			 {"run_hours", Rhrs},
			 {"run_minutes", Rmins},
			 {"run_seconds", Rsecs},
			 {"start_year", Sy},
			 {"start_month", Sm},
			 {"start_day", Sd},
			 {"start_hour", Shr},
			 {"start_minute", Smin},
			 {"start_second", Ssec},
			 {"end_year", Ey},
			 {"end_month", Em},
			 {"end_day", Ed},
			 {"end_hour", Ehr},
			 {"end_minute", Emin},
			 {"end_second", Esec}],
			NL,
			C,
			TCSpec).


write_io_policy(#wrf_cfg{cfg=C, wrf_spec=MS}, NL) ->

    GS = dict:fetch(grib_interval_seconds, C),
    HI = dict:fetch(history_interval_min, C),
    FO = dict:fetch(frames_per_outfile, C),
    RI = dict:fetch(restart_interval_min, C),

    TCSpec = mcfg_spec:nlspec("time_control", MS),

    update_nl_with_list([{"interval_seconds", GS},
			 {"restart_interval", RI},
			 {"history_interval", HI},
			 {"frames_per_outfile", FO}],
			NL,
			C,
			TCSpec).

update_nl_with_list([], NL, _C, _NLSpec) ->
    NL;
update_nl_with_list([{K, V}|R], NL, C, NLSpec) ->
    NL2 = update_namelist(K, V, NL, C, NLSpec),
    update_nl_with_list(R, NL2, C, NLSpec).


update_namelist(K, V, NL, C, NLSpec) ->
    
    % find the entry in the specification an extract
    #nlspec_entry{mult=M} = nlspec:entry(K, NLSpec),
    
    % expand value according to namelist rules
    L = compute_length(M, C),
    {ok, V2} = expand_and_check(V, L),

    % update the namelist with this key/value
    nlist:set_entry(K, V2, NL).


% compute the correct length of the argument
compute_length(max_domains, C) ->
    dict:fetch(domain_num, C);
compute_length(N, _DC) when is_number(N) ->
    N.


expand_and_check(V, L) when is_list(V) ->
    case erlang:length(V) - L of
        0 ->
           {ok, V};
        _ -> 
            {wrong_length, V, L}
    end;

expand_and_check(V, 1) ->
    {ok, V};

expand_and_check(V, L) ->
    {ok, lists:duplicate(L, V)}.
    
