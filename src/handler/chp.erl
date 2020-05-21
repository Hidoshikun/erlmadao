
%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(chp).

-include("logger.hrl").

%% API
-export([
    handler/0
]).

%% ====================================================================
%% API functions
%% ====================================================================
handler() ->
    Url = "https://chp.shadiao.app/api.php",
    case httpc:request(get, {Url, []}, [{ssl, [{verify, 0}]}], []) of
        {ok, {_, _, Body}} ->
            ?WARNING_MSG("Body ~ts ~n", [Body]),
            ok;
        {error, Reason} ->
            ?WARNING_MSG("error reason ~p ~n", [Reason])
    end.

%% ====================================================================
%% Local functions
%% ====================================================================