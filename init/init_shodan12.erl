
% initialize the configuration
wrfx_db:start().
wrfx_db:set_conf(storage_root, "/home/martin/Projects/wrfx/stor").
wrfx_db:set_conf(workspace_root, "/home/martin/Projects/wrfx/workspace").
wrfx_db:set_conf(wrf_34_serial, "/home/martin/Projects/wrf-fire/WRFV3").
wrfx_db:set_conf(default_wps, "/home/martin/Projects/wrf-fire/WPS").
wrfx_db:set_conf(scraper_path, "/home/martin/Projects/wrfx/deps/scraper/extract_observations.py").
wrfx_db:set_conf(moisture_code_path, "/home/martin/Projects/moisture-assimilation/prototypej/run_data_assimilation.jl").
wrfx_db:set_conf(default_geog_root, "/home/martin/Projects/WPS_GEOG").
wrfx_db:set_conf(ncks_path, "/usr/bin/ncks").

% store a small serial job
C = [ {wrf_id, wrf_34_serial},
      {wps_id, default_wps},
      {wrf_exec_method, immediate},
      {geog_root_id, default_geog_root},
      {wps_nl_template_id, colorado_8km_1d_wps_nl},
      {wrf_nl_template_id, colorado_8km_1d_wrf_nl},
%      {from_delta_hr, -24},
%      {to_delta_hr, -16},
      {wrf_from, {{2013, 6, 4}, {0, 0, 0}}},
      {wrf_to, {{2013, 6, 5}, {0, 0, 0}}},
      {grib_interval_seconds, 3600},
      {history_interval_min, 30},
      {grib_sources, [rnrs_nam218]},
      {schedule, {9,0,0}},  % this is in LOCAL TIME!
      {auto_start, false},
      {mf, {wrf_job, execute}}].

wrfx_db:store({job_desc, col_test_run, C}).

C2 = [ {from, {{2013,6,4}, {9, 0, 0}}},
      {to, {{2013, 6, 5}, {0, 0, 0}}},
      {wrfout, "/home/martin/Projects/moisture-assimilation/real_data/colorado_stations/wrf20120601_sel_5km_10mhist.nc"},
      {stations, ["BAWC2", "BBRC2", "BMOC2", "BTAC2", "CCDC2", "CCEC2", "CCYC2", "CHAC2",
		  "CHRC2", "CPPC2", "CPTC2", "CTPC2", "CUHC2", "CYNC2", "DYKC2", "ESPC2",
		  "HACC2", "HRBC2", "JNSC2", "KSHC2", "LKGC2", "LPFC2", "MITC2", "MMRC2",
		  "MOKC2", "MRFC2", "POLC2", "RDDC2", "RDKC2", "RFEC2", "RFRC2", "RRAC2",
		  "RSOC2", "SAWC2", "SDNC2", "SDVC2", "STHC2", "STOC2", "TR563", "TS092",
		  "TS654", "TS761", "TS799", "TS809", "TS864", "TS875", "TS905", "TS925",
		  "TS938", "TS971", "UCNC2", "WLCC2" ]},
       {obs_var_table, [ {"FM", "1e-4"}, {"RELH", "2.5e-3"}, {"TMP", "0.25"} ]},
       {auto_start, false}
      ].

wrfx_db:store({job_desc, moisture_test_run, C2}).


% store the example namelists

NLInstructions = [ {"init/col_8km_namelist.input", colorado_8km_1d_wrf_nl},
                   {"init/col_8km_namelist.wps", colorado_8km_1d_wps_nl},
                   {"init/col_8km_namelist.fire", colorado_8km_1d_fire_nl} ],
lists:foreach(fun ({F, Id}) -> wrfx_db:store({nllist, Id, element(3, nllist:parse(F))}) end, NLInstructions).

io:format("initialization complete.~n").
