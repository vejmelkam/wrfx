
-module(grib_nam_retr).
-author("Martin Vejmelka <vejmelkam@gmail.com").
-export([retrieve/2]).


%
%  Computes which files must be retrieved and
%  immediately returns with the names of the target
%  files.  Starts a process to download the files,
%  which then reports to the sending process.
%


retrieve(From,To) ->
    
    
