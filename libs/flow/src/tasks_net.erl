
-module(tasks_net).
-author("Martin Vejmelka <vejmelkam@gmail.com>").

-export([http_sync_get_stream/2, http_sync_get/1, http_sync_head/1]).


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
	    Msg = io_lib:format("http get ~s success", [URL]),
	    {success, lists:flatten(Msg), Bdy};
	{ok, {{_Ver, 200, _Reason}, Bdy}} ->
	    Msg = io_lib:format("http get ~s success", [URL]),
	    {success, lists:flatten(Msg), Bdy};
	{ok, {{_Ver, Code, Reason}, _Bdy}} ->
	    {failure, io_lib:format("http get ~s failed, server returned ~p with reason ~p", [URL, Code, Reason])};
	{error, R} ->
	    {failure, io_lib:format("http get ~s failed with error ~p", [URL, R])}
    end.


http_sync_head(URL) ->
    case httpc:request(head, {URL, []}, [], []) of
	{ok, {{_Ver, 200, _Reason}, _Hdr, _Bdy}} ->
	    Msg = io_lib:format("http head ~s success, code 200", [URL]),
	    {success, lists:flatten(Msg)};
	{ok, {{_Ver, Code, Reason}, _Hdr, _Bdy}} ->
	    Msg = io_lib:format("http head ~s failed with code ~p and reason ~p", [URL, Code, Reason]),
	    {failure, Msg};
	{error, R} ->
	    Msg = io_lib:format("http head ~s failed with error ~p", [URL, R]),
	    {failure, Msg}
    end.
