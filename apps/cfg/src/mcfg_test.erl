-module(mcfg_test).
-author("vejmelkam@gmail.com").

-include("include/mcfg.hrl").

-export([test/0, test2/0]).

test() ->
   
    WRFDir = "/home/martin/Projects/wrf-fire/WRFV3/Registry",
    MS = mcfg_spec:create_profile_from_reg(WRFDir, vanilla_wrf_v34),

    P = [ {domain_num, 2}, {dt_from, {{2012, 6, 1}, {0, 0, 0}}}, {dt_to, {{2012, 6, 3}, {0, 0, 0}}} ],
    NLS = nlfile:parse("data/namelist.input").


test2() ->
    WRFDir = "/home/martin/Projects/wrf-fire/WRFV3/Registry",
    MS = mcfg_spec:create_profile_from_reg(WRFDir, vanilla_wrf_v34),
    TCS = mcfg_spec:nlspec("time_control", MS),
    io:format("~p~n", [nlspec:entries(TCS)]).
 

