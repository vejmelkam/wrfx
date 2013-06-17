

-module(filesys).
-author("Martin Vejmelka <vejmelkam@gmail.com").
-export([list_dir_regexp/2, file_type/1]).
-include_lib("kernel/include/file.hrl").

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

%% @spec file_type(string()) -> undefined|device|directory|other|regular|symlink
file_type(F) ->
    {ok, FI} = file:read_link_info(F),
    FI#file_info.type.
