

%% information on a namelist entry obtained from a Registry file
-record(nlentry_spec, {nlid, name, type, mult}).

%% model configuration spec
-record(mcfg_spec, {mcfgid, nls}).

%% model configuration
-record(mcfg, {mcfgid,
               time_cfg,
               hist_cfg,
               dt_to,
               dom_cfg,
               path_cfg,
               mcfg_spec}).

%% a namelist entry with filled out details
-record(nlentry, {nlid, name, value}).


%% a path config record
-record(path_cfg, {geog_path, grib_path, wrf_path, out_path}).

%% time configuration record
-record(time_cfg, {from, to}).

%% input configuration
-record(input_cfg, {gribfile_interval_sec,io_form}).

%% output configuration record
-record(output_cfg, {hist_interval,hist_form}).

%% restart config
-record(restart_cfg, {restart, restart_interval, restar_form}).


