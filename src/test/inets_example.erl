-module(inets_example).

%% 网页解析用trane:sax/3和正则

%% API
-export([
    start/0
]).

%% ====================================================================
%% API functions
%% ====================================================================
start() ->
    inets:start(),
    ssl:start(),
    case httpc:request(get, {"https://chp.shadiao.app/api.php", []}, [{ssl, [{verify, 0}]}], []) of
        {ok, {StatusLine, Headers, Body}} ->
            io:format("StatusLine ~p ~n", [StatusLine]),
            io:format("Headers ~p ~n", [Headers]),
            io:format("is list ~p ~n", [is_list(Body)]),
            io:format("Body ~ts ~n", [list_to_binary(Body)]),
            io:format("Body ~p ~n", [list_to_binary(Body)]),
            %% 过滤用的Fun
%%            Fun = fun(T, A) -> A ++ [T] end,
%%            Result = trane:sax(Body, Fun, []),
%%            io:format("Result ~p~n", [Result]),
            ok;
        {error, Reason} ->
            io:format("error reason ~p ~n", [Reason])
    end.
%% ====================================================================
%% Local functions
%% ====================================================================

utf8_list_to_string(List) ->
    unicode:characters_to_binary(unicode:characters_to_list(list_to_binary(List))).