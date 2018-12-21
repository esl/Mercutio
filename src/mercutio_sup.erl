%%%-------------------------------------------------------------------
%%% @author denys.gonchar@erlang-solutions.com
%%% @copyright 2018 Erlang Solutions Ltd.
%%% @doc
%%%   main mercutio supervisor
%%% @end
%%%-------------------------------------------------------------------

-module(mercutio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(APP, mercutio).

%%====================================================================
%% API functions
%%====================================================================
-spec start_link()->{'ok', pid()}|{'error', _}.
start_link() ->
    Config = config:get_config(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @private
-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Config) ->
    ConnectionArgs = proplists:get_value(component_connection_args, Config),
    XmppConnections = #{id       => xmpp_connections,
                        start    => {xmpp_connections_sup, start_link, [ConnectionArgs]},
                        restart  => permanent,
                        shutdown => 5000,
                        type     => supervisor,
                        modules  => [xmpp_connections_sup]},
    Children = [XmppConnections],
    SupervisorFlags = #{strategy  => one_for_all,
                        intensity => 0,
                        period    => 1},
    {ok, {SupervisorFlags, Children}}.

