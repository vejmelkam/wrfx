
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
-export([run_wps/1]).


make_exec_plan(Args) ->

    WPSDir = plist:getp(wps_dir, Args),        % directory with an installation of WPS
    ExecDir = plist:getp(exec_dir, Args),      % directory in which WPS step is supposed to run
    Vtable = plist:getp(vtable_file, Args),    % Vtable file relative to WPS directory
    WPSNL = plist:getp(wps_nl, Args),
    WRFNL = plist:getp(wrf_nl, Args),

    Files = [Vtable, "geogrid.exe", "geogrid", "metgrid.exe", "metgrid", "ungrib.exe", "ungrib"],


    T = [ {filesys_tasks, dir_exists, [WPSDir]},
	  {filesys_tasks, create_dir, [ExecDir]},
	  [ { filesys_tasks, create_symlink, [filename:join(WPSDir, F), filename:join(ExecDir, F)] } || F <- Files ],
	  {filesys_tasks, write_file, [nllist:to_text(WPSNL), filename:join(ExecDir, "namelist.wps")]},
	  {exec_tasks, run_scan_output, filename:join(ExecDir, "geogrid.exe"), "Successful"},
	  [ { filesys_tasks, create_symlink, [X, filename:join(ExecDir, Y)]} || {X,Y} <- make_grib_names(GRIBFiles) ],
	  {exec_tasks, run_scan_output, filename:join(ExecDir, "ungrib.exe"), "Successful"},
	  {exec_tasks, run_scan_output, filename:join(ExecDir, "metgrid.exe"), "Succesful"} ],
	  
    P = #plan{id=wps_exec_plan, tasks=lists:flatten(T)}.
    

run_wps(Args) ->

    WPSDir = plist:getp(wps_dir, Args),        % directory with an installation of WPS
    
    % check if WPS dir exists
    T = [ {filesys_tasks, dir_exists, [WPSDir]} ],
    case plan_runner:execute_plan(#plan{id=wps_check_plan, tasks=T}) of
	{success, _Log} ->
	    run_wps(clone_dir, Args);
	X ->
	    X
    end.


run_wps(clone_dir, Args) ->

    WPSDir = plist:getp(wps_dir, Args),        % directory with an installation of WPS
    ExecDir = plist:getp(exec_dir, Args),      % directory in which WPS step is supposed to run
    Vtable = plist:getp(vtable_file, Args),    % Vtable file relative to WPS directory

    Files = [Vtable, "geogrid.exe", "geogrid", "metgrid.exe", "metgrid", "ungrib.exe", "ungrib"],

    P = clone_dir_planner:make_exec_plan(WPSDir, ExecDir, Files),
    case plan_runner:execute_plan(P) of
	{success, _Log} ->
	    run_wps(store_namelist, Args);
	X ->
	    X
    end;
run_wps(store_namelist, Args) ->
    ExecDir = plist:getp(exec_dir, Args),      % directory in which WPS step is supposed to run
    WPSNL = plist:getp(wps_nl, Args),          % the WPS namelist that is to be stored in namelist.wps
    case filesys_tasks:write_file(filename:join(ExecDir, "namelist.wps"), WPSNL) of
	ok ->
	    run_wps(run_geogrid, Args);
	X ->
	    X
    end;
run_wps(run_geogrid, Args) ->
    ExecDir = plist:getp(exec_dir, Args),      % directory in which WPS step is supposed to run
    Output = os:cmd(filename:join(ExecDir, "geogrid.exe")),
    case string:str("Successful", Output) of
	0 ->
	    {failure, io_lib:format("geogrid failed, output ~p", Output)};
	_ ->
	    run_wps(link_gribs, Args)
    end;
run_wps(link_gribs, Args) ->
    GRIBFiles = plist:getp(grib_files, Args),  % the GRIB files that will be used as input to ungrib.exe
    ExecDir = plist:getp(exec_dir, Args),      % directory in which WPS step is supposed to run
    
    T = [ { filesys_tasks, create_symlink, [X, filename:join(ExecDir, Y)]} || {X,Y} <- make_grib_names(GRIBFiles) ],
    case plan_runner:execute_plan(#plan{id=wps_link_gribs, tasks=T}) of
	{success, _Log} ->
	    run_wps(run_ungrib, Args);
	X ->
	    X
    end;
run_wps(run_ungrib, Args) ->
    ExecDir = plist:getp(exec_dir, Args),      % directory in which WPS step is supposed to run
    Output = os:cmd(filename:join(ExecDir, "ungrib.exe")),
    case string:str("Successful", Output) of
	0 ->
	    {failure, io_lib:format("ungrib stage failed, output ~p", Output)};
	_ ->
	    run_wps(run_metgrid, Args)
    end;  
run_wps(run_metgrid, Args) ->
    ExecDir = plist:getp(exec_dir, Args),      % directory in which WPS step is supposed to run
    Output = os:cmd(filename:join(ExecDir, "metgrid.exe")),
    case string:str("Successful", Output) of
	0 ->
	    {failure, io_lib:format("metgrid stage failed, output ~p", [Output])};
	_ ->
	    {success, "WPS execution successful."}
    end.
    
			 

    
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


