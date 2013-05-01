
-module(nlrdwr).
-author("vejmelkam@gmail.com").

-include("include/mcfg.hrl").

-export([to_wrf_nl/1]).


write_time_range(#mcfg{cfg=C, wrf_nl_spec=MS}, NL) ->

    % both of these must be standard erlang dates
    Ts={{Sy, Sm, Sd}, {Shr, Smin, Ssec}} = dict:fetch(dt_from, C),
    Te={{Ey, Em, Ed}, {Ehr, Emin, Esec}} = dict:fetch(dt_to, C),
    {Rdays, {Rhrs, Rmins, Rsecs}} = calendar:time_difference(Te, Ts),

    TCSpec = mcfg_spec:nlspec("time_control", MS),

    render_cfg_to_nl(["run_days", "run_hours", "run_minutes", "run_seconds",
                      "start_year", "start_month", "start_day",
                      "start_hour", "start_minute", "start_second",
                      "end_year", "end_month", "end_day", "end_hour", "end_minute", "end_second"],
                     [Rdays, Rhrs, Rmins, Rsecs,
                      Sy, Sm, Sd, Shr, Smin, Ssec,
                      Ey, Em, Ed, Ehr, Emin, Esec ],
                     NL,
                     C,
                     TCSpec).


write_io_policy(#mcfg{cfg=C, wrf_nl_spec=MS}, NL) ->

    GS = dict:fetch(grib_interval_seconds, C),
    HI = dict:fetch(history_interval_min, C),
    FO = dict:fetch(frames_per_outfile, C),
    RI = dict:fetch(restart_interval_min, C),

    TCSpec = mcfg_spec:nlspec("time_control", MS),

    update_namelist_from_list([{"interval_seconds", GS},
			       {"restart_interval", RI},
			       {"history_interval", "HI"},
			       {"frames_per_outfile", FO}],
			      NL,
			      C,
			      TCSpec).

update_nl_with_list([], NL, _C, _NLSpec) ->
    NL;
update_nl_with_list([{K, V}|R], NL, C, NLSpec) ->
    NL2 = update_namelist(K, V, NL, C, NLSpec),
    update_namelist_from_list(Keys, Vals, NL2, C, NLSpec).


update_namelist(K, V, NL, C, NLSpec) ->
    
    % find the entry in the specification an extract
    #nlspec_entry{mult=M} = nlspec:entry(K, NLSpec),
    
    % expand value according to namelist rules
    L = compute_length(M, C),
    {ok, V2} = expand_and_check(V, L),

    % update the namelist with this key/value
    nlist:set_entry(K, V, NL).


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
    
