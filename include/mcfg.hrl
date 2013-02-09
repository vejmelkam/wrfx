

%% information on a namelist entry obtained from a Registry file
-record(nlentry_spec, {nlid, name, type, mult}).

%% model configuration spec
-record(mcfg_spec, {mcfgid, nls}).

%% model configuration
-record(mcfg, {mcfgid,
	       cfg,
	       def_wrf_nl,
	       def_wps_nl,
               spec}).

%% a configuration chunk - either internal configuration or namelist
-record(cfg_chunk, {id, kvstore}).

%% a namelist file
-record(nlfile, {id, nlists}).
