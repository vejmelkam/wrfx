

-module(wrf_exec).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([run/1]).




run(Args) ->
    Method = plist:getp(wrf_exec_method, Args),
    S = self(),
    spawn(fun() -> S ! run(Method, Args) end).
    

run(serial, Args) ->
    Dir = plist:getp(wrf_exec_dir, Args),
    
    
