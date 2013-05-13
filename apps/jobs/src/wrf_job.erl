

-module(std_wrf_job).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-export([run_job/1, test_job/0]).

%
%  Required inputs in the Cfg object
%
%    - wps_nl_template: a WPS namelist (as parsed by nllist:parse/1)
%    - wrf_nl_template: a WRF namelist (as parsed by nllist:parse/1)
%    - wrf_root_dir: the directory of the WRF installation to be used
%    - grib_files: list of GRIB files for use by ungrib.exe
%    - vtable_file: WPS-directory relative name of the Vtable file to use with ungrib.exe
% 

test_job() ->
    
    WRFDir = "/home/martin/Projects/wrf-fire",
    WPSTempl = nllist:parse(filename:join(WRFDir, "WPS/namelist.wps")),
    WRFTempl = nllist:parse(filename:join(WRFDir, "WRFV3/run/namelist.input")),
    Cfg1 = wrf_nl:read_config(WRFTempl),

    NLSpec = wrf_reg:create_profile_from_reg(filename:join(WRFDir, "WRFV3/Registry"), vanilla_wrf_v34),

    Files = [ "nam.t00z.awphys00.grb2.tm00", "nam.t00z.awphys01.grb2.tm00",
	      "nam.t00z.awphys02.grb2.tm00", "nam.t00z.awphys03.grb2.tm00" ],
    F2 = lists:map(fun(X) -> filename:join("/home/martin/Projects/GRIB/NAM_20130501", X) end, Files),

    Cfg = plist:update_with([ {wrf_root_dir, WRFDir},
			      {wps_exec_dir, "/home/martin/Temp/wps_temp_anjk4378"},
			      {wps_nl_template, WPSTempl},
			      {wrf_nl_template, WRFTempl},
			      {nl_spec, NLSpec},
			      {dt_from, {{2013, 5, 1}, {0, 0, 0}}},
			      {dt_to, {{2013, 5, 1}, {3, 0, 0}}},
			      {vtable_file, "ungrib/Variable_Tables/Vtable.NAM"},
			      {grib_files, F2} ], Cfg1),

    % delete the test dir
    os:cmd(["rm -rf ", plist:getp(wps_exec_dir, Cfg)]),

    run_job(Cfg).


run_job(Cfg) ->

    % first construct the namelists
    Cfg2 = make_namelists(Cfg),

    %FIXME: download GRIB files here from given source
    % add grib file list to Cfg

    % plan & execute WPS job
    Plan = wps_exec:make_exec_plan(Cfg2),
    io:format("~p~n", [Plan]),

    case plan_runner:execute_plan(Plan) of
     	{success, _Log} ->
     	    run_wrf(Cfg);
	{failure, _MFA, Text, _R, _Log} ->
	    io:format("error during plan execution [~p]~n", [lists:flatten(Text)]),
	    {failure, Text}
    end.


run_wrf(_Cfg) ->
    io:format("Not implemented.").

    % FIXME: enqueue WRF job

    % FIXME: monitor WRF job

    % FIXME: retrieve and store outputs




make_namelists(Cfg) ->
    WPSNL = wps_nl:write_config(Cfg, plist:getp(wps_nl_template, Cfg)),
    WRFNL = wrf_nl:write_config(Cfg, plist:getp(wrf_nl_template, Cfg)),
    plist:update_with([ {wps_nl, WPSNL}, {wrf_nl, WRFNL} ], Cfg).
