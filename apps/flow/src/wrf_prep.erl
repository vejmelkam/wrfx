
%
%  This planner prepares a WRF run.  Prerequisites are:
%
%  - a workspace is available (wrf_exec_dir can be created)
%  - met_em* files have been generated in wps_exec_dir
%  - a namelist for WRF is prepared and available as wrf_nl
%  - a valid run directory is in wrf_root_dir/WRFV3/run
%  - the WRF root directory contains a valid WPS/WRF setup
%

-module(wrf_prep).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("include/flow.hrl").
-export([make_exec_plan/1]).


make_exec_plan(Args) ->

    WPSExecDir = plist:getp(wps_exec_dir, Args),
    WRFDir = filename:join(plist:getp(wrf_root_dir, Args), "WRFV3/run"),  % directory, which is setup to run WRF
    ExecDir = plist:getp(wrf_exec_dir, Args),                             % directory in which WPS step is supposed to run
    WRFNL = plist:getp(wrf_nl, Args),                                     % namelist for wps

    % files that must be symlinked from the workspace directory
    Files = [ "CAM_ABS_DATA", "CAM_AEROPT_DATA", "co2_trans", "ETAMPNEW_DATA", "ETAMPNEW_DATa_DBL",
	      "ETAMPNEW_DATA.expanded_rain", "ETAMPNEW_DATA.expanded_rain_DBL", "GENPARM.TBL",
	      "gribmap.txt", "grib2map.txt", "LANDUSE.TBL", "MPTABLE.TBL", "namelist.fire",
	      "ozone.formatted", "ozone_lat.formatted", "ozone_plev.formatted",
	      "real.exe", "RRTM_DATA", "RRTM_DATA_DBL", "RRTMG_LW_DATA", "RRTMG_LW_DATA_DBL",
	      "RRTMB_SW_DATA", "RRTMG_SW_DATA_DBL", "SOILPARM.TBL", "tc.exe", "tr49t67", "tr49t85"
	      "tr67t85", "URBPARM.TBL", "URBPARM_UZE.TBL", "VEGPARM.TBL", "wrf.exe" ],

    MET_Files = filesys:list_dir_regexp(WPSExecDir, "met_em.+"),

    T = [ {filesys_tasks, create_dir, [ExecDir]},
	  [ { filesys_tasks, create_symlink, [filename:join(WRFDir, F), filename:join(ExecDir, F)] } || F <- Files ],
	  [ { filesys_tasks, create_symlink, [filename:join(WPSExecDir, F), filename:join(ExecDir, F)] } || F <- MET_Files ],
	  {filesys_tasks, write_file, [filename:join(ExecDir, "namelist.input"), nllist:to_text(WRFNL)]},
	  {exec_tasks, run_maybe_mpi, [ExecDir, "./real.exe", "SUCCESS COMPLETE"]} ],
	  
    #plan{id=wrf_prep_exec, tasks=lists:flatten(T)}.
