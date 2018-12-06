%%%-------------------------------------------------------------------
%%% @author denys.gonchar@erlang-solutions.com
%%% @copyright 2018 Erlang Solutions Ltd.
%%% @doc
%%%   this module implements ping pong monitoring. <br/>
%%%   1) if ping_pong process gets ping signals regularly
%%%      it restarts ping timer and does nothing. <br/>
%%%   2) on ping timeout server executes specified PingFn
%%%      and starts pong timer. <br/>
%%%   3) if ping signal arrives before pong timeout, pong timer
%%%      is canceled and ping timer is restarted instead. <br/>
%%%   4) on pong timeout server executes specified PongFn
%%%      and stops execution.
%%% @end
%%%-------------------------------------------------------------------
-module(ping_pong).

-behaviour(gen_server).

%% API
-export([start/4,
         ping/1,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-type timeout_fn() :: fun(()-> any()).

-record(state, {timeout_type = ping :: 'ping' | 'pong',
                ping_timeout :: timeout(),
                pong_timeout :: timeout(),
                ping_fn :: timeout_fn(),   %ping timeout action
                pong_fn :: timeout_fn()}). %pong timeout action


%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% Starts ping/pong monitoring server.
%%% @param PingTimeout ping timeout in milliseconds.
%%% @param PongTimeout pong timeout in milliseconds.
%%% @param PingFn this function is triggered after ping timeout
%%% @param PongFn this function is triggered after pong timeout
%%% @end
%%%-------------------------------------------------------------------
-spec start(timeout(), timeout(), timeout_fn(), timeout_fn()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start(PingTimeout, PongTimeout, PingFn, PongFn) ->
    State = #state{ping_timeout = PingTimeout,
                   pong_timeout = PongTimeout,
                   ping_fn      = PingFn,
                   pong_fn      = PongFn},
    gen_server:start(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%% @end
%%--------------------------------------------------------------------
-spec stop(Pid :: pid()) -> ok.
stop(Pid) -> gen_server:cast(Pid, stop).

%%--------------------------------------------------------------------
%% @doc
%% Restarts the ping timer. Cancels pong timer, if required.
%% @end
%%--------------------------------------------------------------------
-spec ping(Pid :: pid()) -> ok.
ping(Pid) -> gen_server:cast(Pid, ping).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init(State) ->
    {ok, State}.

%% @private
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_call(Request, From, State) ->
    {stop, {unknown_call, From, Request}, unknown_call, State}.

%% @private
-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(ping, #state{ping_timeout = Timeout} = State) ->
    {noreply, State#state{timeout_type = ping}, Timeout};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

%% @private
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info(timeout, #state{timeout_type = ping, ping_fn = PingFn,
                            pong_timeout = Timeout} = State) ->
    PingFn(),
    {noreply, State#state{timeout_type = pong}, Timeout};
handle_info(timeout, #state{timeout_type = pong, pong_fn = PongFn} = State) ->
    PongFn(),
    {stop, pong_timeout, State};
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.
