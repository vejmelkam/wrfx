
-module(clone_dir_planner).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-export([make_plan/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


make_symlink_task(Src, Dst, F) ->
    Ex = filename:join(Src, F),
    New = filename:join(Dst, F),
    {filesys_tasks, create_symlink, [Ex, New]}.


make_plan(Args) ->
    
    % retrieve input arguments from plist
    Src = plist:getp(src_dir, Args),
    Dst = plist:getp(dst_dir, Args),
    Files = plist:getp(with_files, Args),

    % construct plan
    [ { filesys_tasks, check_dir_exists, [Src]},
      { filesys_tasks, create_dir, [Dst]} |
      lists:reverse(lists:foldl(fun(F, A) -> [make_symlink_task(Src, Dst, F)|A] end, [], Files)) ].


-ifdef(TEST).

clone_dir_plan_test() ->
    A = [ {src_dir, "/home/martin/Temp/t1"},
	  {dst_dir, "/home/martin/Temp/t2"},
	  {with_files, ["file1", "file2", "readme"]} ],
    P = make_plan(A),
    file:write_file("plan.test", io_lib:format("~p~n", [P])).

-endif.
