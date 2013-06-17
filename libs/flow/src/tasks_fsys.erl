

%% @doc
%% Module containing tasks that primarily involve the file system.
%% @end

-module(tasks_fsys).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([dir_exists/1, create_dir/1, delete_dir/1]).
-export([file_exists/1, write_file/2, delete_file/1, create_symlink/2,
	 rename_file/2, delete_files_regexp/2]).
-export([delete/1]).


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


%% @doc Removes a file from the filesystem, wraps {@link file:delete/1}.
%% @spec delete_file(F::string()) -> {success, string()}|{failure, string()}
delete_file(F) ->
    case file:delete(F) of
	ok ->
	    {success, io_lib:format("file [~s] deleted", [F])};
	{error, enoent} ->
	    {success, io_lib:format("file [~s] is non-existent, skipping", [F])};
	{error, E} ->
	    {failure, io_lib:format("failed to delete file ~s with error ~p", [F, E])}
    end.


%% @doc Remove a directory from the filesystem.  The directory need not be empty beforehand.
%% @spec delete_dir(F::string()) -> {success, string()} | {failure, string()}
delete_dir(F) ->
    case file:list_dir(F) of
	{ok, Fs} ->
	    case lists:foldl(fun (X, {success, _}) -> delete(filename:join(F, X));
				 (_X, {failure, E}) -> {failure, E} end, {success, []}, Fs) of
		{success, _} ->
		    case file:del_dir(F) of
			ok ->
			    {success, io_lib:format("directory ~s successfully removed", [F])};
			{error, R} ->
			    {failure, io_lib:format("failed to remove directory ~s with error ~p", [F, R])}
		    end;
		Failure ->
		    Failure
	    end;
	{error, enoent} ->
	    % if directory doesnt exist, it's just a no-op
	    {success, io_lib:format("directory ~s did not exist", [F])};
	{error, E} ->
	    {failure, io_lib:format("failed to remove directory ~s with error ~p", [F, E])}
    end.


%% @doc Remove a file or a directory from the filesystem.  The directory need not be empty beforehand.
%% @spec delete(F::string()) -> {success, string()} | {failure, string()}
delete(F) ->
    case filesys:file_type(F) of
	directory ->
	    delete_dir(F);
	symlink ->
	    delete_file(F);
	regular ->
	    delete_file(F);
	X ->
	    {failure, io_lib:format("attempting to delete file [~s] of type ~p", [F, X])}
    end.


%% @doc Creates a symlink to a file (file must exist).
%% @spec create_symlink(F::string(), L::string()) -> {success, string()}|{failure, string()}
create_symlink(F, L) ->
    case filelib:is_file(L) of
	true ->
	    io:format("FILE ~p is found to exist~n", [L]),
	    {failure, io_lib:format("a file [~s] already exists", [L])};
	false ->
	    io:format("FILE ~p is found NOT to exist~n", [L]),
	    case file:make_symlink(F, L) of
		ok ->
		    {success, io_lib:format("created symlink [~s] -> [~s]", [L, F])};
		{error, E} ->
		    {failure, io_lib:format("failed to create symlink [~s] -> [~s] with error [~p]", [L, F, E])}
	    end
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




rename_file(Orig, New) ->
    case file:rename(Orig, New) of
	ok ->
	    {success, io_lib:format("file [~s] renamed to [~s]", [Orig, New])};
	{error, R} ->
	    {failure, io_lib:format("failed to rename [~s] to [~s] with error ~p", [Orig, New, R])}
    end.


%% @doc Delete all files in directory D which conform to regular expression RE.
%% @spec delete_files_regexp(D::string(), RE::string()) -> {success, []} | {error, Reason}
delete_files_regexp(D, RE) ->
    Fs = filesys:list_dir_regexp(D, RE),
    lists:foldl(fun (X, {success, _}) -> delete_file(filename:join(D,X));
		    (_X, {failure, R}) -> {failure, R} end, {success, []}, Fs).
			 
