
-module(filesys_tasks).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([dir_exists/1, file_exists/1,
	 create_dir/1, create_symlink/2,
	 write_file/2, delete_file/1]).


file_exists(F) ->
    case filelib:is_file(F) of
	true ->
	    {success, io_lib:format("file ~p exists.", [F])};
	false ->
	    {failure, io_lib:format("file ~p does not exist.", [F])}
    end.


dir_exists(D) ->
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


write_file(Fname, Content) ->
    case file:write_file(Fname, Content) of
	ok ->
	    {success, io_lib:format("content written to [~p].~n", [Fname])};
	{error, R} ->
	    {failure, io_lib:format("could not write to [~p] with error [~p].~n", [Fname, R])}
    end.


delete_file(Fname) ->
    case file:delete(Fname) of
	ok ->
	    {success, io_lib:format("file [~p] deleted.~n", [Fname])};
	{error, enoent} ->
	    {success, io_lib:format("file [~p] is non-existent, skipping.~n", [Fname])};
	{error, E} ->
	    {failure, io_lib:format("error [~p] encountered while deleting file [~p]~n", [E, Fname])}
    end.
