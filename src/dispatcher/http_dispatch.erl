-module(http_dispatch).

-export([init/2]).
-export([terminate/3]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello World!">>, Req0),
    io:format("http connect ~n"),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    io:format("connect closed ~n"),
    ok.
