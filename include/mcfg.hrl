

%% information on a namelist entry obtained from a Registry file
-record(nlentry_spec, {nlid, name, type, mult}).

%% model configuration spec
-record(mcfg_spec, {mcfgid, nls}).

%% model configuration
-record(mcfg, {mcfgid,
               time_chunk,
               hist_chunk,
               dom_chunk,
               io_chunk,
               restart_chunk,
               mcfg_spec}).

%% a configuration chunk (spatial, domain, domains, input, output, restart, etc.)
-record(cfg_chunk, {id, kvstore}).

