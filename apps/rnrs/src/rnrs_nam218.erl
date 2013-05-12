
%
%  A resolver computes storage ids and URLs for GRIB files.
%  Due to operational constraints, it may happen that current
%  cycle run files are unavailable, in this case, older cycle
%  run files may be requested using CycleRunDelta > 0 parameter.
%
%  This resolver computes a manifest for the NAM grid #218
%  84-hr forecast source.
%

-module(rnrs_nam218).
-author("Martin Vejmelka <vejmelkam@gmail.com").
-export([storage_prefix/0, url_prefix/0, manifest/2]).


storage_prefix() ->
    "nam_218".


url_prefix() ->
    "http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod/".


manifest(_From, _To, _CycleRunDelta) ->
    [].
