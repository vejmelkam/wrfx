

%% information on a namelist entry obtained from a Registry file
-record(nlentry_spec, {nlid, name, type, mult}).

%% model configuration spec
-record(mcfg_spec, {mcfgid, nls}).

%% model configuration
-record(mcfg, {mcfgid, cfg, mcfg_spec}).

%% a namelist entry with filled out details
-record(nlentry, {nlid, name, value, nlentry_spec}).




