

%% namelist specifications read in from the Registry
-record(nlspec, {id, entries}).

%% information on a namelist entry obtained from the Registry
-record(nlspec_entry, {nlid, name, type, mult}).

%% model configuration spec
-record(mcfg_spec, {mcfgid, nls}).

%% model configuration
-record(mcfg, {mcfgid,       % id of the configuration
	       cfg,          % a dictionary of key-value pairs that stores wrfx configuration
	       def_wrf_nl,   % a tuple representing the base namelist (modified according to cfg)
	       def_wps_nl,   % a tuple representing the base wps namelist  (modified according to cfg)
               wrf_nl_spec}).% a namelist specification according to which the wrf namelist is filled out

%% a namelist file containing zero or more namelists as property lists (see plist)
-record(nlfile, {id, nls}).

% a single namelist containing zero or more entries
-record(nl, {id, entries}).
