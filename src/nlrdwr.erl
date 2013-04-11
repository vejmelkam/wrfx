
-module(nlrdwr).
-author("vejmelkam@gmail.com").

-include("include/mcfg.hrl").

-export([to_wrf_nl/1]).


to_wrf_nl(C) ->
    NLWRF = dict:new(),

    NL = write_from_to(C, []),
    NL2 = write_io_policy(C, NL),
    
    dict:store("time_control", NL2, NLWRF).


write_from_to(#mcfg{cfg=C, spec=MS}, NL) ->

    % both of these must be standard erlang dates
    Ts={{Sy, Sm, Sd}, {Shr, Smin, Ssec}} = cfg_chunk:get(dt_from, C),
    Te={{Ey, Em, Ed}, {Ehr, Emin, Esec}} = cfg_chunk:get(dt_to, C),
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

write_io_policy(#mcfg{cfg=C,spec=MS}, NL) ->

    GS = cfg_chunk:get(grib_interval_seconds, C),
    HI = cfg_chunk:get(history_interval_min, C),
    FO = cfg_chunk:get(frames_per_outfile, C),
    R = cfg_chunk:get(restart, C),
    RI = cfg_chunk:get(restart_interval_min, C),
    DL = cfg_chunk:get(debug_level, C),

    TCSpec = mcfg_spec:nlspec("time_control", MS),

    render_cfg_to_nl(["interval_seconds", "restart", "restart_interval", "history_interval", "frames_per_outfile",
                    "io_form_history", "io_form_restart", "io_form_input", "io_form_boundary", "debug_level"],
                     [GS, R, RI, HI, FO, 2, 2, 2, 2, DL],
                     NL,
                     C,
                     TCSpec).



render_cfg_to_nl([], [], A, _DC, _NLSpec) ->
    A;

render_cfg_to_nl([K|Vars], [V|Vals], A, DC, NLSpec) ->

    % find the entry in the specification an extract
    io:format("~p~n", [nlspec:entry(K, NLSpec)]),
    #nlspec_entry{mult=M} = nlspec:entry(K, NLSpec),
    
    % expand value according to namelist rules
    L = compute_length(M, DC),
    {ok, V2} = expand_and_check(V, L),

    % add to the namelist
    A2 = [{K, V2}|A],

    render_cfg_to_nl(Vars, Vals, A2, DC, NLSpec).


% compute the correct length of the argument
compute_length(max_domains, DC) ->
    cfg_chunk:get(domain_num, DC);
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
    
