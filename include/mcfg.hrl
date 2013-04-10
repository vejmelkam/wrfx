

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

%% a namelist file containing zero or more namelists as property lists (see plist)
-record(nlfile, {id, nls}).

% a single namelist containing zero or more entries
-record(nl, {id, entries}).
