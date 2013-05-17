

-module(wrf_exec).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([run/1]).




run(Args) ->
    BuildType = plist:getp(wrf_build_type, Args),
    ExecMethod = plist:getp(wrf_exec_method, Args),

    S = self(),
    spawn(fun() -> S ! run(ExecMethod, Args) end).
    

run(serial_run, Args) ->
    Dir = plist:getp(wrf_exec_dir, Args);


run(mpi_exec, Args) ->
    Dir = plist:getp(wrf_exec_dir, Args);


run(submit_job, Args) ->
    Dir = plist:getp(wrf_exec_dir, Args).
    
    
    
    
