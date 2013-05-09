
-module(clone_dir_planner).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("include/flow.hrl").
-export([make_exec_plan/1, make_check_plan/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


make_check_plan(Args) ->
    % source directory must exist
    Src = plist:getp(src_dir, Args),

    #plan{id = clone_dir_check,
	  tasks = [ {filesys_tasks, check_dir_exists, [Src] } ]}.



make_exec_plan(Args) ->
    
    % retrieve input arguments from plist
    Src = plist:getp(src_dir, Args),
    Dst = plist:getp(dst_dir, Args),
    Files = plist:getp(with_files, Args),

    % construct plan
    #plan{id = clone_dir_exec, 
	  tasks = [ {filesys_tasks, create_dir, [Dst]} |
		    [ make_symlink_task(Src, Dst, F) || F <- Files ] ]}.

make_cleanup_plan(Args) ->
    Dst = plist:getp(dst_dir, Args),
    #plan{id = clone_dir_cleanup,
	  tasks = [ {filesys_tasks, remove_dir, [Dst]} ]}.
    

make_symlink_task(Src, Dst, F) ->
    Ex = filename:join(Src, F),
    New = filename:join(Dst, F),
    {filesys_tasks, create_symlink, [Ex, New]}.


-ifdef(TEST).

clone_dir_plan_test() ->
    A = [ {src_dir, "/home/martin/Temp/t1"},
	  {dst_dir, "/home/martin/Temp/t2"},
	  {with_files, ["file1", "file2", "readme"]} ],
    P = make_exec_plan(A),
    file:write_file("plan.test", io_lib:format("~p~n", [P])).

-endif.
