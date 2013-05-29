
%% @doc
%%  This subsystem runs the WPS module assuming that
%%
%%  - a workspace is available (exec_dir can be created)
%%  - GRIB files are available
%%  - the Vtable file is known for the GRIB data
%%  - a namelist for WPS is available
%%  - an installation of WPS exists in wps_dir
%%  - the WRF root directory contains a valid WPS/WRF setup (GEOGRID.TBL pointing to the right file, etc.)
%%
%%
%%  The plan produces in the exec_dir met_em* files which must
%%  be linked into the directory in which wrf.exe is executed.
%% @end

-module(plan_wps_exec).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("include/flow.hrl").
-export([make_exec_plan/1]).


make_exec_plan(Args) ->

    WPSDir = plist:getp(wps_install_dir, Args),             % directory with an installation of WPS
    ExecDir = plist:getp(wps_exec_dir, Args),               % directory in which WPS step is supposed to run
    Vtable = plist:getp(vtable_file, Args),                 % Vtable file relative to WPS directory
    WPSNL = plist:getp(wps_nl, Args),                       % namelist for wps
    GRIBFiles = plist:getp(grib_files, Args),               % list of GRIB files

    % files that must be symlinked from the workspace directory
    Files = ["geogrid.exe", "geogrid", "metgrid.exe", "metgrid", "ungrib.exe", "ungrib"],

    T = [ % create target directory
	  {tasks_fsys, create_dir, [ExecDir]},

	  % symlink files from install to execution directory
	  [ { tasks_fsys, create_symlink,
	      [filename:join(WPSDir, F), filename:join(ExecDir, F)] } || F <- Files ],

	  % symlink vtable (depends on GRIB file source)
	  { tasks_fsys, create_symlink,
	    [filename:join(WPSDir, Vtable), filename:join(ExecDir, "Vtable")] },

	  % write the constructed WPS namelist into namelist.wps
	  {tasks_fsys, write_file,
	   [filename:join(ExecDir, "namelist.wps"), nllist:to_text(WPSNL)]},

	  % run geogrid.exe, store output in geogrid.output
	  {tasks_exec, execute,
	   [filename:join(ExecDir, "geogrid.exe"), 
	    [{output_type, stdout},  {in_dir, ExecDir},
	     {exit_check, {scan_for, "Successful completion of geogrid"}},
	     {store_output_to, filename:join(ExecDir, "geogrid.output")}]]},

	  % link in all grib files with correct names GRIBFILE.AAA
	  [ { tasks_fsys, create_symlink,
	      [X, filename:join(ExecDir, Y)]} || {X,Y} <- make_grib_names(GRIBFiles) ],

	  % execute ungrib.exe, store output in ungrib.output
	  {tasks_exec, execute,
	   [filename:join(ExecDir, "ungrib.exe"),
	    [{output_type, stdout},  {in_dir, ExecDir},
	     {exit_check, {scan_for, "Successful completion of ungrib"}},
	     {store_output_to, filename:join(ExecDir, "ungrib.output")}]]},

	  % execute metgrid.exe, store output in metgrid.output
	  {tasks_exec, execute,
	   [filename:join(ExecDir, "metgrid.exe"),
	    [{output_type, stdout},  {in_dir, ExecDir},
	     {exit_check, {scan_for, "Successful completion of metgrid"}},
	     {store_output_to, filename:join(ExecDir, "metgrid.output")}]]} ],
	  
    #plan{id=wps_exec, tasks=lists:flatten(T)}.
    

make_grib_names(GF) ->
    make_names(GF, $A, $A, $A, []).

make_names([], _, _, _, P) ->
    P;
%note $Z+1 = 91, erlang parser does not like pattern $Z+1
make_names(_G, 91, _E2, _E3, _P) ->
    too_many_grib_files;
make_names(G, E1, 91, E3, P) ->
    make_names(G, E1+1, $A, E3, P);
make_names(G, E1, E2, 91, P) ->
    make_names(G, E1, E2+1, $A, P);
make_names([G|GF], E1, E2, E3, P) ->
    make_names(GF, E1, E2, E3+1, [{G, "GRIBFILE." ++ [E1, E2, E3]}|P]).


