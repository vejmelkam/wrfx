

-module(util_tasks).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([sleep_ms/1]).



sleep_ms(T) ->
    timer:sleep(T),
    {success, io_lib:format("Waited for [~p] ms.", [T])}.
