

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

%% a namelist (sections is list of tuples {id, dict}, each of which represents a section in the namelist file)
-record(nl, {id, sections}).
