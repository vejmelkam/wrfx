

%% @doc
%% Module containing tasks that primarily involve the file system.
%% @end

-module(tasks_fsys).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([dir_exists/1, file_exists/1,
	 create_dir/1, create_symlink/2,
	 write_file/2, delete_file/1,
	 rename_file/2]).


%% @doc Checks if a file exists (as a regular file) on the filesystem.
%% Wraps {@link filelib:is_regular/1}.
%% @spec file_exists(F::string()) -> {success, string()}|{failure, string()}
file_exists(F) ->
    case filelib:is_regular(F) of
	true ->
	    {success, io_lib:format("file ~s exists", [F])};
	false ->
	    {failure, io_lib:format("file ~s does not exist", [F])}
    end.


%% @doc Checks if a directory exists on the filesystem.
%% @spec dir_exists(D::string()) -> {success, string()}|{failure, string()}
dir_exists(D) ->
    case filelib:is_dir(D) of
	true ->
	    {success, io_lib:format("source directory ~s exists", [D])};
	false ->
	    {failure, check_dir_exists_task, io_lib:format("source directory ~s does not exist", [D])}
    end.

%% @doc Create a directory on the filesystem.
%% @spec create_dir(D::string()) -> {success, string()}|{failure, string()}
create_dir(D) ->
    case filelib:ensure_dir(filename:join(D, "test_file")) of
	ok ->
	    {success, io_lib:format("destination directory ~s created", [D])};
	{error, E} ->
	    {failure, io_lib:format("cannot create directory ~s, reported error [~s]", [D, E])}
    end.


%% @doc Creates a symlink to a file (file must exist).
%% @spec create_symlink(F::string(), L::string()) -> {success, string()}|{failure, string()}
create_symlink(F, L) ->
    case file:make_symlink(F, L) of
	ok ->
	    {success, io_lib:format("created symlink [~s] -> [~s]", [L, F])};
	{error, E} ->
	    {failure, io_lib:format("failed to create symlink [~s] -> [~s] with error [~p]", [L, F, E])}
    end.


%% @doc Writes the content to a file.  The directory of the file must exist.  Wraps {@link file:write_file/2}.
%% @spec write_file(F::string(), C::iodata()) -> {success, string()}|{failure, string()}
write_file(F, C) ->
    case file:write_file(F, C) of
	ok ->
	    {success, io_lib:format("content written to [~s]", [F])};
	{error, R} ->
	    {failure, io_lib:format("could not write to [~s] with error [~p]", [F, R])}
    end.


%% @doc Removes a file from the filesystem, wraps {@link file:delete/1}.
%% @spec delete_file(F::string()) -> {success, string()}|{failure, string()}
delete_file(F) ->
    case file:delete(F) of
	ok ->
	    {success, io_lib:format("file [~s] deleted", [F])};
	{error, enoent} ->
	    {success, io_lib:format("file [~s] is non-existent, skipping", [F])};
	{error, E} ->
	    {failure, io_lib:format("error [~s] encountered while deleting file [~p]", [E, F])}
    end.


rename_file(Orig, New) ->
    case file:rename(Orig, New) of
	ok ->
	    {success, io_lib:format("file [~s] renamed to [~s]", [Orig, New])};
	{error, R} ->
	    {failure, io_lib:format("failed to rename [~s] to [~s] with error ~p", [Orig, New, R])}
    end.
