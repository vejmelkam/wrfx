

-module(std_wrf_job).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-export([run_job/1]).


run_job(Cfg) ->

    % first construct the namelists
    Cfg2 = make_namelists(Cfg),

    %FIXME: download GRIB files here from given source



make_namelists(Cfg) ->
    WPSNL = wps_nl:write_config(plist:getp(wps_nl_template, Cfg)),
    WRFNL = wrf_nl:write_config(plist:getp(wrf_nl_template, Cfg)),
    plist:update_with([ {wps_nl, WPSNL}, {wrf_nl, WRFNL} ]).
