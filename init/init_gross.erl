
% initialize the configuration
wrfx_db:start().
wrfx_db:set_conf(storage_root, "/home/mvejmelka/Projects/wrfx/stor").
wrfx_db:set_conf(workspace_root, "/home/mvejmelka/Projects/wrfx/workspace").
wrfx_db:set_conf(wrf_34_mpi, "/home/mvejmelka/Packages/op-wrf-fire/WRFV3").
wrfx_db:set_conf(default_wps, "/home/mvejmelka/Packages/op-wrf-fire/WPS").
wrfx_db:set_conf(default_geog_root, "/home/mvejmelka/Packages/WPS_GEOG").
wrfx_db:set_conf(ncks_path, "/opt/lib/bin/ncks").

% store the operational forecast job
C = [ {wrf_id, wrf_34_mpi},
      {wps_id, default_wps},
      {geog_root_id, default_geog_root},
      {wrf_exec_method, immediate},
      {wps_nl_template_id, colorado_2km_1d_wps_nl},
      {wrf_nl_template_id, colorado_2km_1d_wrf_nl},
      {from_delta_hr, -24},
      {to_delta_hr, 36},
      {grib_interval_seconds, 3600},
      {history_interval_min, 30},
      {grib_sources, [rnrs_nam218]},
      {mpi_exec_name, "/usr/mpi/gcc/openmpi-1.4.3/bin/mpiexec"},
      {mpi_nprocs, 12*4},
      {mpi_nodes, [ "node09", "node10", "node11", "node12" ]},
      {schedule, {9, 0, 0}},
      {auto_start, false},
%      {ncks_prune_wrfout, ["Times", "T2", "PSFC", "XLAT", "XLONG", "Q2", "RAINNC", "RAINC", "HGT"]},
      {mf, {wrf_job, execute}}].

wrfx_db:store({job_desc, colorado_2km_op_wrf_48hr_run, C}).


Ct = [ {wrf_id, wrf_34_mpi},
       {wps_id, default_wps},
       {geog_root_id, default_geog_root},
       {wrf_exec_method, immediate},
       {wps_nl_template_id, colorado_8km_1d_wps_nl},
       {wrf_nl_template_id, colorado_8km_1d_wrf_nl},
       {from_delta_hr, 0},
       {to_delta_hr, 1},
       {grib_interval_seconds, 3600},
       {history_interval_min, 30},
       {grib_sources, [rnrs_nam218]},
       {mpi_exec_name, "/usr/mpi/gcc/openmpi-1.4.3/bin/mpiexec"},
       {mpi_nprocs, 12*4},
       {mpi_nodes, [ "node09", "node10", "node11", "node12" ]},
       {schedule, now},
       {auto_start, false},
%       {ncks_prune_wrfout, ["Times", "T2", "PSFC", "XLAT", "XLONG", "Q2", "RAINNC", "RAINC", "HGT"]},
       {mf, {wrf_job, execute}}].

wrfx_db:store({job_desc, test_job, Ct}).


C2 = [ {wrf_id, wrf_34_mpi},
       {wps_id, default_wps},
       {geog_root_id, default_geog_root},
       {wrf_exec_method, immediate},
       {wps_nl_template_id, colorado_2km_1d_wps_nl},
       {wrf_nl_template_id, colorado_2km_1d_wrf_nl},
       {from_delta_hr, 0},
       {to_delta_hr, 2},
       {grib_interval_seconds, 3600},
       {history_interval_min, 30},
       {grib_sources, [rnrs_nam218]},
       {mpi_exec_name, "/usr/mpi/gcc/openmpi-1.4.3/bin/mpiexec"},
       {mpi_nprocs, 12*4},
       {mpi_nodes, [ "node09", "node10", "node11", "node12" ]},
       {schedule, {20, 0, 0}},
       {auto_start, false},
 %      {ncks_prune_wrfout, ["Times", "T2", "PSFC", "XLAT", "XLONG", "Q2", "RAINNC", "RAINC", "HGT"]},
       {mf, {wrf_job, execute}}].

wrfx_db:store({job_desc, colorado_2km_short_run, C2}).

C3 = [ {from, {{2013,6,19}, {9, 0, 0}}},
       {to, {{2013,6,21}, {9, 0, 0}}},
       {wrfout, "/home/mvejmelka/Projects/wrfx/stor/outputs/colorado_2km_op_wrf_48hr_run_2013-06-19_09\:00\:00/wrfout_d01_2013-06-19_09\:00\:00"},
       {stations, ["BAWC2", "BBRC2", "BMOC2", "BTAC2", "CCDC2", "CCEC2", "CCYC2", "CHAC2",
		   "CHRC2", "CPPC2", "CPTC2", "CTPC2", "CUHC2", "CYNC2", "DYKC2", "ESPC2",
		   "HACC2", "HRBC2", "JNSC2", "KSHC2", "LKGC2", "LPFC2", "MITC2", "MMRC2",
		   "MOKC2", "MRFC2", "POLC2", "RDDC2", "RDKC2", "RFEC2", "RFRC2", "RRAC2",
		   "RSOC2", "SAWC2", "SDNC2", "SDVC2", "STHC2", "STOC2", "TR563", "TS092",
		   "TS654", "TS761", "TS799", "TS809", "TS864", "TS875", "TS905", "TS925",
		   "TS938", "TS971", "UCNC2", "WLCC2" ]},
       {obs_var_table, [ {"FM", "1e-4"}, {"RELH", "2.5e-3"}, {"TMP", "0.25"} ]},
       {auto_start, false},
       {mf, {moisture_job, execute}}
     ].

wrfx_db:store({job_desc, moisture_colorado_run, C3}).


C4 = [ {wrf_job_id, colorado_2km_short_run},
       {moisture_job_id, moisture_colorado_run},
       {wrfout_base, "wrfout_d01_"},
       {vars, "T2,RH,RAIN,FM1,FM10,FM100"},
       {riak_host, "localhost"},
       {riak_port, "10017"},
       {auto_start, false},
       {mf, {fire_danger_job, execute}} ].

wrfx_db:store({job_desc, fire_danger_operational_test, C4}).


C5 = [ {wrf_job_id, colorado_2km_op_wrf_48hr_run},
       {moisture_job_id, moisture_colorado_run},
       {wrfout_base, "wrfout_d01_"},
       {vars, "T2,RH,RAIN,FM1,FM10,FM100"},
       {riak_host, "localhost"},
       {riak_port, "10017"},
       {schedule, {9, 0, 0}},
       {auto_start, true},
       {mf, {fire_danger_job, execute}} ].

wrfx_db:store({job_desc, fire_danger_operational_v1, C5}).


% store the example namelists
NLInstructions = [ {"init/col_2km_namelist.input", colorado_2km_1d_wrf_nl},
		   {"init/col_2km_namelist.wps", colorado_2km_1d_wps_nl},
		   {"init/col_2km_namelist.fire", colorado_2km_1d_fire_nl},
		   {"init/col_8km_namelist.input", colorado_8km_1d_wrf_nl},
		   {"init/col_8km_namelist.wps", colorado_8km_1d_wps_nl},
		   {"init/col_8km_namelist.fire", colorado_8km_1d_fire_nl} ],
lists:foreach(fun ({F, Id}) -> wrfx_db:store({nllist, Id, element(3, nllist:parse(F))}) end, NLInstructions).
