

%% list of namelist specifications stored as a dictionary
-record(wrf_spec, {id, nls}).

%% namelist specifications read in from the Registry (for one namelist)
-record(nlspec, {id, entries}).

%% information on a namelist entry obtained from the Registry
-record(nlspec_entry, {nlid, name, type, mult}).

%% model configuration
-record(wrf_cfg,
	{mcfgid,         % id of the configuration
	 cfg,            % a dictionary of pairs storing wrfx configuration
	 wrf_spec        % id of namelist specification (depends on WRF version)
	}). 

%% a namelist list containing zero or more namelists as property lists (see plist)
-record(nllist, {id, nls}).

% a single namelist containing zero or more entries
-record(nl, {id, entries}).
