
-module(filesys_tasks).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-export([check_dir_exists/1, create_dir/1, create_symlink/2]).


check_dir_exists(D) ->
    case filelib:is_dir(D) of
	true ->
	    {success, io_lib:format("source directory ~p exists.", [D])};
	false ->
	    {failure, check_dir_exists_task, io_lib:format("source directory ~p does not exist.", [D])}
    end.


create_dir(D) ->
    case filelib:ensure_dir(filename:join(D, "test_file")) of
	ok ->
	    {success, io_lib:format("destination directory ~p created.", [D])};
	{error, E} ->
	    {failure, io_lib:format("cannot create directory ~p, reported error [~p]~n", [D, E])}
    end.


create_symlink(Existing, New) ->
    case file:make_symlink(Existing, New) of
	ok ->
	    {success, io_lib:format("created symlink [~p] -> [~p].~n", [New, Existing])};
	{error, E} ->
	    {failure, io_lib:format("failed to create symlink [~p] -> [~p] with error [~p].~n", [New, Existing, E])}
    end.