%%%-------------------------------------------------------------------
%%% @author denys.gonchar@erlang-solutions.com
%%% @copyright 2018 Erlang Solutions Ltd.
%%% @doc
%%%   supervisor for xmpp connections
%%% @end
%%%-------------------------------------------------------------------
-module(xmpp_connections_sup).
-author("denys").

-behaviour(supervisor).

%% API
-export([start_link/1,
         get_random_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_COMPONENT_PORT, 8888).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link(list()) -> {ok, pid()}|{error, _}.
start_link(ConnectionArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, ConnectionArgs).

-spec get_random_child() -> {term(), pid()}.
get_random_child() ->
    Children = supervisor:which_children(?SERVER),
    Count = length(Children),
    RandomChild = lists:nth(rand:uniform(Count), Children),
    {Child, Pid, _ProcessType, [_Module]} = RandomChild,
    {Child, Pid}.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(ConnectionArgs) ->
    Children = generate_children_list(ConnectionArgs),
    SupervisorFlags = #{strategy  => one_for_one,
                        intensity => 1,
                        period    => 1},
    {ok, {SupervisorFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_children_list(ConnectionArgs) ->
    MimHosts = proplists:get_value(mim_hosts, ConnectionArgs),
    ChildArgs = proplists:delete(mim_hosts, ConnectionArgs),
    [#{id       => Host ++ ":" ++ integer_to_list(Port),
       start    => {echo_component, start_link,
                    [[{host, Host}, {port, Port} | ChildArgs]]},
       restart  => permanent,
       shutdown => 5000,
       type     => worker,
       modules  => [echo_component]} || {Host, Port} <- split_hosts(MimHosts)].

split_hosts(MimHosts) ->
    [case string:split(HostAndPort, ":", all) of
         [Host] -> {Host,?DEFAULT_COMPONENT_PORT};
         [Host,Port] -> {Host,list_to_integer(Port)}
     end || HostAndPort <- string:split(MimHosts, ";", all)].