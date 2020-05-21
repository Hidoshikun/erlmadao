-module(websocket_dispatch).

-include("logger.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).
-export([terminate/3]).

init(Req, _State0) ->
    ?WARNING_MSG("this is a warning"),
    State = ["hello"],
    {cowboy_websocket, Req, State}.

%% 建立websocket连接
websocket_init(State) ->
    {reply, {text, <<"Hello!">>}, State}.

%% 处理websocket连接
websocket_handle({text, Msg}, State) ->
    ?WARNING_MSG("receive msg ~p ~n", [Msg]),
    {reply, {text, <<"That's what she said! ", Msg/binary>>}, State};
websocket_handle(_Data, State) ->
    {reply, State}.

%% 处理收到的erlang消息
websocket_info({timeout, _Ref, Msg}, State) ->
    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {reply, State}.

%% 断开websocket连接
websocket_terminate(_Reason, _Req, _State) ->
    ok.
terminate(_Reason, _Req, _State) ->
    io:format("connect closed ~n"),
    ok.
