
-module(filesys_tasks).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-export([check_dir_exists/1, create_dir/1, create_symlink/2]).


check_dir_exists(D) ->
    T = {filesys_tasks, check_dir_exists, [D]},
    case filelib:is_dir(D) of
	true ->
	    {success, T, io_lib:format("source directory ~p exists.", [D])};
	false ->
	    {failure, T, check_dir_exists_task, io_lib:format("source directory ~p does not exist.", [D])}
    end.


create_dir(D) ->
    T = {filesys_tasks, create_dir, [D]},
    case filelib:ensure_dir(D) of
	ok ->
	    {success, T, io_lib:format("destination directory ~p created.", [D])};
	{error, E} ->
	    {failure, T, io_lib:format("clone_dir: FATAL: cannot create directory ~p, reported error [~p]~n", [D, E])}
    end.


create_symlink(Existing, New) ->
    T = {filesys_tasks, create_dir, [Existing, New]},
    case file:make_symlink(Existing, New) of
	ok ->
	    {success, T, io_lib:format("created symlink [~p] -> [~p].~n", [New, Existing])};
	{error, E} ->
	    {failure, T, io_lib:format("failed to create symlink [~p] -> [~p] with error [~p].~n", [New, Existing, E])}
    end.
