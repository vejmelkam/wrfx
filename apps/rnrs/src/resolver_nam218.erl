
%
%  A resolver computes storage ids and URLs for GRIB files.
%
%  This resolver computes a manifest for the NAM grid #218
%  84-hr forecast source.
%


-module(resolver_nam218).
-author("Martin Vejmelka <vejmelkam@gmail.com").
-export([storage_prefix/0, url_prefix/0, manifest/2]).


storage_prefix() ->
    "nam_218".


url_prefix() ->
    "http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod/".


manifest(From, To) ->
    [].
