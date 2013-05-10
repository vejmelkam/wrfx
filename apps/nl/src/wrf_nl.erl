
%
%  This module tranforms specific configuration key/value pairs
%  in the wrf_cfg record and a given namelist into
%  a namelist with modifications required by the config.
%
%  Also, the module reads in information from a namelist into
%  a configuration object that summarizes relevant parts of the namelist.
%

-module(wrf_nl).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("include/wrf_cfg.hrl").
-export([read_config/1, write_config/2,
	 read_time_range/2, write_time_range/2,
	 read_io_policy/2, write_io_policy/2,
	 read_domain_info/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


read_config(NL) ->
    % read in configuration parts we need to know to manipulate this NL further

    TC = nllist:namelist("time_control", NL),

    P = read_time_range(TC, plist:new()),
    P2 = read_domain_info(nllist:namelist("domains", NL), P),
    read_io_policy(TC, P2).


read_time_range(TC, P) ->

    TS = {list_to_tuple(nlist:entries(["start_year", "start_month", "start_day"], TC)),
	  list_to_tuple(nlist:entries(["start_hour", "start_minute", "start_second"], TC))},
    TE = {list_to_tuple(nlist:entries(["end_year", "end_month", "end_day"], TC)),
	  list_to_tuple(nlist:entries(["end_hour", "end_minute", "end_second"], TC))},

    plist:update_with(P, [{dt_from, TS}, {dt_to, TE}]).


read_domain_info(Dom, P) ->
    plist:setp(num_domains, lists:nth(1, nlist:entry("max_dom", Dom)), P).


read_io_policy(TC, P) ->
    E = nlist:entries(["interval_seconds", "restart_interval", "history_interval", "frames_per_outfile"], TC),
    plist:update_with([grib_interval_seconds, restart_interval_min, history_interval_min, frames_per_outfile], E, P).


write_config(Cfg, NL) ->
    NL2 = write_time_range(Cfg, NL),
    write_io_policy(Cfg, NL2).


write_time_range(Cfg, NL) ->

    % both of these must be standard erlang dates
    Ts = {{Sy, Sm, Sd}, {Shr, Smin, Ssec}} = plist:getp(dt_from, Cfg),
    Te = {{Ey, Em, Ed}, {Ehr, Emin, Esec}} = plist:getp(dt_to, Cfg),
    {Rdays, {Rhrs, Rmins, Rsecs}} = calendar:time_difference(Ts, Te),

    Spec = plist:getp(nl_spec, Cfg),
    TC = nllist:namelist("time_control", NL),

    TC2 = update_namelist([{"run_days", Rdays}, {"run_hours", Rhrs}, {"run_minutes", Rmins}, {"run_seconds", Rsecs},
			   {"start_year", Sy}, {"start_month", Sm}, {"start_day", Sd},
			   {"start_hour", Shr}, {"start_minute", Smin}, {"start_second", Ssec},
			   {"end_year", Ey}, {"end_month", Em}, {"end_day", Ed},
			   {"end_hour", Ehr}, {"end_minute", Emin}, {"end_second", Esec}],
			  TC,
			  Cfg,
			  wrf_reg:nlspec("time_control", Spec)),

    nllist:set_namelist("time_control", TC2, NL).
    


write_io_policy(Cfg, NL) ->

    TC = nllist:namelist("time_control", NL),

    GS = plist:getp(grib_interval_seconds, Cfg),
    HI = plist:getp(history_interval_min, Cfg),
    FO = plist:getp(frames_per_outfile, Cfg),
    RI = plist:getp(restart_interval_min, Cfg),
    Spec = plist:getp(nl_spec, Cfg),

    TC2 = update_namelist([{"interval_seconds", GS},
			   {"restart_interval", RI},
			   {"history_interval", HI},
			   {"frames_per_outfile", FO}],
			  TC,
			  Cfg,
			  wrf_reg:nlspec("time_control", Spec)),

    nllist:set_namelist("time_control", TC2, NL).

update_namelist([], NL, _Cfg, _NLSpec) ->
    NL;
update_namelist([{K, V}|R], NL, Cfg, NLSpec) ->
    update_namelist(R, update_namelist(K, V, NL, Cfg, NLSpec), Cfg, NLSpec).


update_namelist(K, V, NL, Cfg, NLSpec) ->
    
    % find the entry in the specification an extract
    #nlspec_entry{mult=M} = nlspec:entry(K, NLSpec),
    
    % expand value according to namelist rules
    L = compute_length(M, Cfg),
    {ok, V2} = expand_and_check(V, L),

    % update the namelist with this key/value
    nlist:set_entry(K, V2, NL).


% compute the correct length of the argument
compute_length(max_domains, Cfg) ->
    plist:getp(num_domains, Cfg);
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
    {ok, [V]};

expand_and_check(V, Len) ->
    {ok, lists:duplicate(Len, V)}.
    



-ifdef(TEST).

second_test() ->
    plist:new().


config_read_write_wrf_test() ->
   
    % construct a WRFv3.4 registry profile object
    WRFDir = "/home/martin/Projects/wrf-fire/WRFV3/Registry",
    MS = wrf_reg:create_profile_from_reg(WRFDir, vanilla_wrf_v34),

    % load a namelist from WRF v3.4
    NLS = nllist:parse("../data/namelist.input"),
    P = read_config(NLS),

    % construct a new configuration (this is a plist)
    P2 = [ {dt_from, {{2012, 6, 1}, {0, 0, 0}}}, {dt_to, {{2012, 6, 3}, {0, 0, 0}}} ],
    Wcfg = #wrf_cfg{cfg = plist:update_with(P2, P), wrf_spec = MS},

    NLS2 = write_time_range(Wcfg, NLS),
    T = nllist:to_text(NLS2),
    file:write_file("../data/namelist.input.constructed", T).

-endif.
