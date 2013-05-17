
-module(network_tasks).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-export([http_sync_get/2]).


http_sync_get(URL, Tgt) ->
    case httpc:request(get, {URL, []}, [], [{stream, Tgt}]) of
	{ok, saved_to_file} ->
	    {success, io_lib:format("d/l [~p], stored as [~p]~n", [URL, Tgt])};
	{stream_to_file_failed, R} ->
	    {failure, io_lib:format("d/l [~p] succeeded, stream to file [~p] failed with reason [~p]~n", [URL, Tgt, R])};
	{ok, {_Ver, 404, _Info}, _Opts, _Bdy} ->
	    {failure, io_lib:format("target url [~p] does not exist~n", [URL])};
	{error, E} ->
	    {failure, io_lib:format("failed to d/l [~p] and store as [~p] with error [~p]~n", [URL, Tgt, E])}
    end.

