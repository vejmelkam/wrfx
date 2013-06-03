%
%  This module tranforms specific configuration key/value pairs
%  in the wps_cfg record and a given namelist into
%  a namelist with modifications required by the config.
%
%  Also, the module reads in information from a namelist into
%  a configuration object that summarizes relevant parts of the namelist.
%

-module(wps_nl).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("include/wrf_cfg.hrl").
-export([read_config/1, write_config/2,
	 read_share/2, write_share/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


read_config(NL) ->
    Sh = nllist:namelist("share", NL),
    read_share(Sh, plist:new()).


read_share(Sh, P) ->

    TS = esmf:parse_time(lists:nth(1, nlist:entry("start_date", Sh))),
    TE = esmf:parse_time(lists:nth(1, nlist:entry("end_date", Sh))),
    IS = lists:nth(1, nlist:entry("interval_seconds", Sh)),
    ND = lists:nth(1, nlist:entry("max_dom", Sh)),
    plist:update_with([{wps_from, TS}, {wps_to, TE}, {grib_interval_seconds, IS}, {num_domains, ND}], P).


write_config(Cfg, NL) ->
    write_share(Cfg, NL).


write_share(Cfg, NL) ->

    % both of these must be standard erlang dates
    Ts = plist:getp(wps_from, Cfg),
    Te = plist:getp(wps_to, Cfg),
    IS = plist:getp(grib_interval_seconds, Cfg),
    ND = plist:getp(num_domains, Cfg),

    Sh = nllist:namelist("share", NL),

    Sh2 = update_namelist([{"start_date", esmf:time_to_string(Ts)}, {"end_date", esmf:time_to_string(Te)},
			   {"interval_seconds", IS}, {"max_dom", ND}],
			  Sh),

    nllist:set_namelist("share", Sh2, NL).
    

update_namelist(UP, NL) ->
    lists:foldl(fun ({K,V},A) -> nlist:set_entry(K, [V], A) end, NL, UP).



-ifdef(TEST).

config_read_write_wps_test() ->
   
    % load a namelist from WRF v3.4
    NLS = nllist:parse("../data/namelist.wps"),
    P = read_config(NLS),

    plist:store("../data/namelist.wps.cfg", P),

    % construct a new configuration (this is a plist)
    P2 = [ {wps_from, {{2012, 6, 1}, {0, 0, 0}}}, {wps_to, {{2012, 6, 3}, {0, 0, 0}}} ],
    Wcfg = plist:update_with(P2, P),

    NLS2 = write_config(Wcfg, NLS),
    T = nllist:to_text(NLS2),
    file:write_file("../data/namelist.wps.constructed", T).

-endif.
