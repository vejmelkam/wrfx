
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
      {geog_root, default_geog_root},
      {wrf_exec_method, immediate},
      {wps_nl_template_id, colorado_2km_1d_wps_nl},
      {wrf_nl_template_id, colorado_2km_1d_wrf_nl},
      {from_delta_hr, -24},
      {to_delta_hr, 24},
      {grib_interval_seconds, 3600},
      {history_interval_min, 30},
      {grib_sources, [rnrs_nam218]},
      {mpi_exec_name, "/usr/mpi/gcc/openmpi-1.4.3/bin/mpiexec"},
      {mpi_nprocs, 12*4},
      {mpi_nodes, [ "node09", "node10", "node11", "node12" ]},
      {schedule, {9, 0, 0}},
      {auto_start, true},
      {ncks_prune_wrfout, ["Times", "T2", "PSFC", "XLAT", "XLONG", "Q2", "RAINNC", "RAINC", "HGT"]},
      {mf, {wrf_job, execute}}].

wrfx_db:store({job_desc, colorado_2km_op_wrf_48hr_run, C}).


Ct = [ {wrf_id, wrf_34_mpi},
       {wps_id, default_wps},
       {geog_root, default_geog_root},
       {wrf_exec_method, immediate},
       {wps_nl_template_id, colorado_8km_1d_wps_nl},
       {wrf_nl_template_id, colorado_8km_1d_wrf_nl},
       {from_delta_hr, 0},
       {to_delta_hr, 2},
       {grib_interval_seconds, 3600},
       {history_interval_min, 30},
       {grib_sources, [rnrs_nam218]},
       {mpi_exec_name, "/usr/mpi/gcc/openmpi-1.4.3/bin/mpiexec"},
       {mpi_nprocs, 12*4},
       {mpi_nodes, [ "node09", "node10", "node11", "node12" ]},
       {schedule, now},
       {auto_start, false},
       {ncks_prune_wrfout, ["Times", "T2", "PSFC", "XLAT", "XLONG", "Q2", "RAINNC", "RAINC", "HGT"]},
       {mf, {wrf_job, execute}}].

wrfx_db:store({job_desc, test_job, Ct}).


C2 = [ {wrf_id, wrf_34_mpi},
       {wps_id, default_wps},
       {wrf_exec_method, immediate},
       {wps_nl_template_id, colorado_2km_1d_wps_nl},
       {wrf_nl_template_id, colorado_2km_1d_wrf_nl},
       {from_delta_hr, 0},
       {to_delta_hr, 5},
       {grib_interval_seconds, 3600},
       {history_interval_min, 30},
       {grib_sources, [rnrs_nam218]},
       {mpi_exec_name, "/usr/mpi/gcc/openmpi-1.4.3/bin/mpiexec"},
       {mpi_nprocs, 12*4},
       {mpi_nodes, [ "node09", "node10", "node11", "node12" ]},
       {schedule, {8, 0, 0}},
       {auto_start, false},
       {ncks_prune_wrfout, ["Times", "T2", "PSFC", "XLAT", "XLONG", "Q2", "RAINNC", "RAINC", "HGT"]},
       {mf, {wrf_job, execute}}].

wrfx_db:store({job_desc, colorado_2km_1hr_run, C2}).


% store the example namelists
NLInstructions = [ {"init/col_2km_namelist.input", colorado_2km_1d_wrf_nl},
		   {"init/col_2km_namelist.wps", colorado_2km_1d_wps_nl},
		   {"init/col_2km_namelist.fire", colorado_2km_1d_fire_nl},
		   {"init/col_8km_namelist.input", colorado_8km_1d_wrf_nl},
		   {"init/col_8km_namelist.wps", colorado_8km_1d_wps_nl},
		   {"init/col_8km_namelist.fire", colorado_8km_1d_fire_nl} ],
lists:foreach(fun ({F, Id}) -> wrfx_db:store({nllist, Id, element(3, nllist:parse(F))}) end, NLInstructions).
