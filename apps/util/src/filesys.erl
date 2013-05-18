

-module(filesys).
-author("Martin Vejmelka <vejmelkam@gmail.com").
-export([list_dir_regexp/2, remove_directory/1]).


list_dir_regexp(Dir, RegExp) ->
    {ok, MP} = re:compile(RegExp),
    {ok, FN} = file:list_dir(Dir),
    lists:filter(fun (X) -> is_match_re(X, MP) end, FN).


is_match_re(X, MP) ->
    case re:run(X, MP) of
	{match, _} ->
	    true;
	nomatch ->
	    false
    end.


remove_directory(D) ->
    case file:list_dir(D) of
	{ok, F} ->
	    lists:foreach(fun (X) -> file:delete(filename:join(D, X)) end, F),
	    file:del_dir(D);
	{error, enoent} ->
	    % if directory doesnt exist, it's just a no-op
	    ok;
	{error, E} ->
	    {failure, E}
    end.
			   
