
-module(mcfg).
-author(vejmelkam@gmail.com).

-include("include/mcfg.hrl").
-export([default_registry_files/0, parse_registry/2]).




% Returns the standard registry list that should be parsed to generate
% configuration information for WRF.
%
default_registry_files() ->
    [ 'Registry.EM_COMMON',
      'registry.dimspec',
      'registry.fire',
      'registry.avgflx',
      'registry.stoch',
      'registry.les',
      'registry.cam',
      'registry.ssib',
      'registry.fire' ].


% Parse a list of registry files (all in the same directory), obtain
% all the namelist entries there and return them as a dict of lists
% by namelist name.
parse_registry(Dir,Flist) ->
    Fpaths = lists:map(fun(X) -> filename:join(Dir,X) end, Flist),
    Entries = lists:map(fun reg_parser:parse_file/1, Fpaths),
    insert_entries(dict:new(), lists:flatten(Entries)).


% Insert all nlentries in list into a depth 2 tree of dictionaries for easy access later.
%
insert_entries(D, []) ->
    D;
insert_entries(D, [E=#nlentry{nlid=NLid,name=Name}|R]) ->
    case dict:is_key(NLid, D) of
        true ->
            D2 = dict:update(NLid, fun (NL) -> dict:store(Name, E, NL) end, D),
            insert_entries(D2, R);
        false ->
            NL = dict:append(Name, E, dict:new()),
            insert_entries(dict:store(NLid, NL, D), R)
    end.


