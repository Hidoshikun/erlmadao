
%%%-------------------------------------------------------------------
%%% 定时器gen_server
%%%-------------------------------------------------------------------
-module(mod_timer).

-behaviour(gen_server).

-include("logger.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Args) ->
    try
        do_init(Args)
    catch
        _:Reason ->
            ?ERROR_MSG("~p init is exception:~w", [?MODULE, Reason]),
            ?ERROR_MSG("get_stacktrace:~n~p", [erlang:get_stacktrace()]),
            {stop, Reason}
    end.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch
        _:Reason ->
            ?ERROR_MSG("~p handle_call is exception:~w~nRequest:~w", [?MODULE, Reason, Request]),
            ?ERROR_MSG("get_stacktrace:~n~p", [erlang:get_stacktrace()]),
            {reply, ok, State}
    end.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}        (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    try
        do_cast(Msg, State)
    catch
        _:Reason ->
            ?ERROR_MSG("~p handle_cast is exception:~w~nMsg:~w", [?MODULE, Reason, Msg]),
            ?ERROR_MSG("get_stacktrace:~n~p", [erlang:get_stacktrace()]),
            {noreply, State}
    end.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}        (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch
        _:Reason ->
            ?ERROR_MSG("~p handle_info is exception:~w~nInfo:~w", [?MODULE, Reason, Info]),
            ?ERROR_MSG("get_stacktrace:~n~p", [erlang:get_stacktrace()]),
            {noreply, State}
    end.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    try
        do_terminate(Reason, State)
    catch
        _:Reason ->
            ?ERROR_MSG("~p terminate is exception:~w", [?MODULE, Reason]),
            ?ERROR_MSG("get_stacktrace:~n~p", [erlang:get_stacktrace()]),
            ok
    end.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Local Functions
%% ====================================================================
%% --------------------------------------------------------------------
do_init(_Args) ->
    process_flag(trap_exit, true),
    ?WARNING_MSG("~p init...", [?MODULE]),
    
    %% 整点定时器
    {_, M, S} = time(),
    Interval = 60 * 60 - 60 * M - S,
    erlang:send_after(Interval * 1000, self(), "whole_hour"),
    
    {ok, #state{}}.

%% --------------------------------------------------------------------
do_call(Request, _, State) ->
    ?WARNING_MSG("~p call is not match:~w", [?MODULE, Request]),
    {reply, ok, State}.

%% --------------------------------------------------------------------
do_cast(Msg, State) ->
    ?WARNING_MSG("~p cast is not match:~w", [?MODULE, Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
do_info("whole_hour", State) ->
    ?WARNING_MSG("now time ~p", [now()]),
    do_whole_hour_task(),
    erlang:send_after(60 * 60 * 1000, self(), "whole_hour"),
    {noreply, State};
do_info(Info, State) ->
    ?WARNING_MSG("~p info is not match:~w", [?MODULE, Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
do_terminate(Reason, _State) ->
    ?WARNING_MSG("~p terminate...~nReason:~p", [?MODULE, Reason]),
    ok.

%% --------------------------------------------------------------------
%% 整点定时任务
do_whole_hour_task() ->
    ?WARNING_MSG("whole hour task here ").