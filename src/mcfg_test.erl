-module(mcfg_test).
-author(vejmelkam@gmail.com).

-include("include/mcfg.hrl").

-export([test/0, test2/0]).

test() ->
   
    WRFDir = "/home/martin/Projects/wrf-fire/WRFV3/Registry",

    DC = cfg_chunk:new(domain_chunk, [ {domain_num, 2} ]),
    CH = cfg_chunk:new(time_chunk, [ {dt_from, {{2012, 6, 1}, {0, 0, 0}}}, {dt_to, {{2012, 6, 3}, {0, 0, 0}}} ]),
    IO = cfg_chunk:load("data/io_chunk.def"),

    MS = mcfg_spec:create_profile_from_reg(WRFDir, vanilla_wrf_v34),
    MCFG = #mcfg{mcfgid=default, time_chunk = CH, dom_chunk = DC, mcfg_spec=MS, io_chunk=IO},

    cfg_wrfnl:to_wrf_nl(MCFG).

test2() ->

    WRFDir = "/home/martin/Projects/wrf-fire/WRFV3/Registry",
    MS = mcfg_spec:create_profile_from_reg(WRFDir, vanilla_wrf_v34),
    TCS = mcfg_spec:nlspec("time_control", MS),
    io:format("~p~n", [nlspec:entries(TCS)]).
 

