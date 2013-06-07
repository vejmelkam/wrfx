
-module(tasks_net).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-export([http_sync_get_stream/2, http_sync_get/1]).


http_sync_get_stream(URL, Tgt) ->
    case httpc:request(get, {URL, []}, [], [{stream, Tgt}]) of
	{ok, saved_to_file} ->
	    {success, io_lib:format("d/l [~p], stored as [~p]", [URL, Tgt])};
	{stream_to_file_failed, R} ->
	    {failure, io_lib:format("d/l [~p] succeeded, stream to file [~p] failed with reason [~p]", [URL, Tgt, R])};
	{ok, {_Ver, Code, Reason}, _Opts, _Bdy} ->
	    {failure, io_lib:format("http get ~s failed with error ~p and reason ~p", [URL, Code, Reason])};
	{error, E} ->
	    {failure, io_lib:format("failed to d/l [~p] and store as [~p] with error [~p]", [URL, Tgt, E])}
    end.



http_sync_get(URL) ->
    case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
	{ok, {{_Ver, 200, _Reason}, _Hdrs, Bdy}} ->
	    {success, io_lib:format("http get ~s success", [URL]), Bdy};
	{ok, {{_Ver, 200, _Reason}, Bdy}} ->
	    {success, io_lib:format("http get ~s success", [URL]), Bdy};
	{ok, {{_Ver, Code, Reason}, _Bdy}} ->
	    {failure, io_lib:format("http get ~s failed, server returned ~p with reason ~p", [URL, Code, Reason])};
	{error, R} ->
	    {failure, io_lib:format("http get ~s failed with error ~p", [URL, R])}
    end.
