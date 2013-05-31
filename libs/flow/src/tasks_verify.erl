

-module(tasks_verify).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([check_keys/2, namelist_exists/1, config_exists/1]).


check_keys(Ks, P) ->
    case plist:contains_all(Ks, P) of
	{false, Missing} ->
	    {failure, io_lib:format("Missing keys ~p", [Missing])};
	true ->
	    {success, io_lib:format("Keys ~p are in property list", [Ks])}
    end.


namelist_exists(Id) ->
    case wrfx_db:lookup({nllist, Id}) of
	{success, _} ->
	    {success, io_lib:format("Namelist ~p found in database", [Id])};
	{failure, R} ->
	    {failure, io_lib:format("Namelist ~p not found in database, error ~p", [Id, R])}
    end.
    
config_exists(C) ->
    try
	wrfx_db:get_conf(C),
	{success, io_lib:format("Config key ~s exists", [C])}
    catch
	_ ->
	    {failure, io_lib:format("Configu key ~s not found", [C])}
    end.
