
-module(nl_scanner).
-author("vejmelkam@gmail.com").
-export([scan/1]).


scan(FName) ->
    {ok, D} = file:open(FName, [read]),
    L = read_lines(D, []),
    file:close(D),
    scan_lines(L).


scan_lines(L) ->
    scan_lines(L, [], 1).

scan_lines([], T, N) ->
%    lists:reverse([{'$end',N}|T]);
    lists:reverse(T);
scan_lines([L|R], T, N) ->
    % scan tokens on this line (which are separated by whitespace, strings are inside ' pairs)
    {ok, RE} = re:compile("(/)|(&)|(=)|([\\w\\.]+)|'(.+)'|(,)"),
    LT = re:split(L, RE, [{return, list}]),
    LF = lists:filter(fun valid_token/1, LT),
    T2 = lists:foldl(fun (X, A) -> [process_token(X,N)|A] end, T, LF),
    scan_lines(R, T2, N+1).

insert_entry_dividers(T) ->
    insert_entry_dividers(T, []).
insert_entry_dividers([{'=',N1}, {string,N2,Key}, {',', N3}|R], A) ->
    insert_entry_dividers(R, [{';', N3}, {string,N2,Key}, {'=', N1} | A]);
insert_entry_dividers([{'/', N1}, {',',N2}|R], A) ->
    insert_entry_dividers(R, [{';',N2},{'/',N1}|A]);
insert_entry_dividers([T|R], A) ->
    insert_entry_dividers(R, [T|A]);
insert_entry_dividers([], A) ->
    lists:reverse(A).


valid_token([]) ->	
    false;
valid_token("\n") ->
    false;
valid_token(T) ->
    case string:strip(T) of
	[] ->
	    false;
	_T ->
	    true
    end.


process_token("=", N) ->
    {'=', N};
process_token("&", N) ->
    {'&', N};
process_token(",", N) ->
    {',', N};
process_token("/", N) ->
    {'/', N};
process_token(T, N) ->
    {string, N, T}.


read_lines(D, A) ->
    case file:read_line(D) of
	eof ->
	    lists:reverse(A);
	{ok, L} ->
	    read_lines(D, [strip_line(L)|A])
    end.


strip_line([]) ->
    [];
strip_line([$!|_R]) ->
    [];
strip_line(L) ->
    case string:chr(L, $!) of
	0 ->
	    string:strip(L);
	P ->
	    string:strip(string:sub_string(L, 1, P-1))
    end.
