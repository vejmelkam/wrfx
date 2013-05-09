
%
%  This subsystem runs the WPS module assuming that
%  - a workspace is available (exec_dir can be created)
%  - GRIB files are available
%  - the Vtable file is known for the GRIB data
%  - a namelist for WPS is available
%  - an installation of WPS exists in wps_dir
%

-module(wps_exec).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("include/flow.hrl").
-export([make_exec_plan/1]).


make_exec_plan(Args) ->

    RootDir = plist:getp(wrf_root_dir, Args),  % root of WRF installation
    WPSDir = filename:join(RootDir, "WPS"),    % directory with an installation of WPS
    ExecDir = plist:getp(wps_exec_dir, Args),  % directory in which WPS step is supposed to run
    Vtable = plist:getp(vtable_file, Args),    % Vtable file relative to WPS directory
    WPSNL = plist:getp(wps_nl, Args),          % namelist for wps
    GRIBFiles = plist:getp(grib_files, Args),  % list of GRIB files

    Files = ["geogrid.exe", "geogrid", "metgrid.exe", "metgrid", "ungrib.exe", "ungrib"],

    T = [ {filesys_tasks, dir_exists, [WPSDir]},
	  {filesys_tasks, create_dir, [ExecDir]},
	  [ { filesys_tasks, create_symlink, [filename:join(WPSDir, F), filename:join(ExecDir, F)] } || F <- Files ],
	  { filesys_tasks, create_symlink, [filename:join(WPSDir, Vtable), filename:join(ExecDir, "Vtable")] },
	  {filesys_tasks, write_file, [filename:join(ExecDir, "namelist.wps"), nllist:to_text(WPSNL)]},
	  {exec_tasks, run_scan_output, [ExecDir, "./geogrid.exe", "Successful"]},
	  [ { filesys_tasks, create_symlink, [X, filename:join(ExecDir, Y)]} || {X,Y} <- make_grib_names(GRIBFiles) ],
	  {exec_tasks, run_scan_output, [ExecDir, "./ungrib.exe", "Successful"]},
	  {exec_tasks, run_scan_output, [ExecDir, "./metgrid.exe", "Successful"]} ],
	  
    #plan{id=wps_exec_plan, tasks=lists:flatten(T)}.
    

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


