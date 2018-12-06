%%%-------------------------------------------------------------------
%%% @author denys.gonchar@erlang-solutions.com
%%% @copyright 2018 Erlang Solutions Ltd.
%%% @doc
%%%   mercutio application interfaces
%%% @end
%%%-------------------------------------------------------------------

-module(mercutio_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%%   application behaviour callback
%%% @end
%%%-------------------------------------------------------------------
-spec start(_,_) -> {ok, pid()} | {error,_}.
start(_StartType, _StartArgs) ->
    mercutio_sup:start_link().

%%%-------------------------------------------------------------------
%%% @doc
%%%   application behaviour callback
%%% @end
%%%-------------------------------------------------------------------
-spec stop(_) -> ok.
stop(_State) ->
    ok.

