
-module(mcfg_spec).
-author("vejmelkam@gmail.com").

-include("include/mcfg.hrl").
-export([default_registry_files/0, create_profile_from_reg/2, create_profile_from_reg/3,nlslist/1,nlspec/2,to_string/1,from_string/1,load/1]).


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


create_profile_from_reg(Dir,McfgId) ->
    create_profile_from_reg(Dir, default_registry_files(), McfgId).

% Parse a list of registry files (all in the same directory), obtain
% all the namelist entries there and return them as a dict of lists
% by namelist name.
create_profile_from_reg(Dir,Flist,McfgId) ->
    Fpaths = lists:map(fun(X) -> filename:join(Dir,X) end, Flist),
    Entries = lists:map(fun reg_parser:parse_file/1, Fpaths),

    % add some entries that are in io_boilerplate registry which has ifdefs and whatnot
    Entries2 = [ #nlspec_entry{nlid="time_control", name="restart", type=logical, mult=1},
                 #nlspec_entry{nlid="time_control", name="restart_interval", type=integer, mult=1},
                 #nlspec_entry{nlid="time_control", name="history_interval", type=integer, mult=1} | Entries ],

    #mcfg_spec{mcfgid=McfgId, nls=insert_entries(dict:new(), lists:flatten(Entries2))}.


% Insert all nlentries in list into a depth 2 tree of dictionaries for easy access later.
%
insert_entries(D, []) ->
    D;
insert_entries(D, [E=#nlspec_entry{nlid=NLid,name=Name}|R]) ->
    case dict:is_key(NLid, D) of
        true ->
            D2 = dict:update(NLid, fun (NL) -> dict:store(Name, E, NL) end, D),
            insert_entries(D2, R);
        false ->
            NL = dict:store(Name, E, dict:new()),
            insert_entries(dict:store(NLid, NL, D), R)
    end.


% Retrieve a list of namelists that exist in the mcfg_spec.
nlslist(#mcfg_spec{nls=C}) ->
    dict:fetch_keys(C).

% Retrieve a namelist specification
nlspec(Name, #mcfg_spec{nls=C}) ->
    #nlspec{id=Name, entries=dict:fetch(Name, C)}.

% Convert the namelist spec to a string representation
to_string(C) ->
    R = io_lib:format("~p.",[C]),
    lists:flatten(R).

% Read a string representation into an erlang structure
from_string(S) ->
    {ok,Tokens,_} = erl_scan:string(S),
    {ok,C} = erl_parse:parse_term(Tokens),
    C.


load(F) ->
    {ok, B} = file:read_file(F),
    S = binary:bin_to_list(B),
    from_string(S).
