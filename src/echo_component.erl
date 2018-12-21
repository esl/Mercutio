-module(echo_component).

-include_lib("exml/include/exml_stream.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-behavior(escalus_component).

%% API
-export([start_link/1,
         send_stanza/2,
         send_stanza/1]).

%% escalus_component behaviour API
-export([init/1,
         terminate/2,
         process_stanza/3]).

-define(DEFAULT_PING_TIMEOUT, 30000).
-define(DEFAULT_PONG_TIMEOUT, 45000).


-record(state, {ping_pong_pid :: pid()}).

-type state() :: #state{}.

-spec start_link(list()) -> {ok, pid()}.
start_link(ConnectionArgs) ->
    XmppDomain = proplists:get_value(xmpp_domain, ConnectionArgs),
    DomainPrefix = proplists:get_value(domain_prefix, ConnectionArgs),
    ComponentSubDomain = <<DomainPrefix/bitstring, ".", XmppDomain/bitstring>>,
    ComponentConnectionArgs = [{component, ComponentSubDomain} |
                               [proplists:lookup(Key, ConnectionArgs) ||
                                   Key <- [host, port, password]]],
    erlang:display(ComponentConnectionArgs),
    PingTimeout = proplists:get_value(ping_timeout, ConnectionArgs, ?DEFAULT_PING_TIMEOUT),
    PongTimeout = proplists:get_value(pong_timeout, ConnectionArgs, ?DEFAULT_PONG_TIMEOUT),
    InitArgs = [PingTimeout, PongTimeout, ComponentSubDomain, XmppDomain],
    {ok, Component} = escalus_component:start_link(echo_component,
                                                   ComponentConnectionArgs,
                                                   InitArgs),

    escalus_component:set_filter(Component,
                                 fun(Stanza) ->
                                     %% just print and accept all stanzas
                                     io:format("~nIncomingStanza = ~p~n", [Stanza]),
                                     true
                                 end),
    {ok, Component}.

-spec send_stanza(pid(), exml:element()) -> ok.
send_stanza(XMPPComponentPid, Msg) ->
    escalus_component:send(XMPPComponentPid, Msg).

%%--------------------------------------------------------------------
%% @doc
%% sends message to the randomly selected xmpp component connection.
%% as long as all the connections are done to one and the same
%% cluster, the message will reach the recipient.
%% @end
%%--------------------------------------------------------------------
-spec send_stanza(exml:element()) -> ok.
send_stanza(Msg) ->
    {_Child, XMPPComponentPid} = xmpp_connections_sup:get_random_child(),
    escalus_component:send(XMPPComponentPid, Msg).


%%====================================================================
%% escalus_component behaviour API
%%====================================================================

%% @private
-spec init(term()) -> {ok, state()}.
init([PingTimeout, PongTimeout, ComponentSubDomain, XmppDomain]) ->
    Self = self(),
    PingStanza1 = escalus_stanza:ping_request(XmppDomain),
    PingStanza = escalus_stanza:from(PingStanza1, ComponentSubDomain),
    PingFn = fun() -> echo_component:send_stanza(Self, PingStanza) end,
    PongFn = fun() -> escalus_component:stop(Self, pong_timeout) end,
    {ok, Pid} = ping_pong:start(PingTimeout, PongTimeout, PingFn, PongFn),
    ping_pong:ping(Pid),
    InitialState = #state{ping_pong_pid = Pid},
    {ok, InitialState}.

%% @private
-spec process_stanza(exml:element(), escalus_client:client(), state()) -> {ok, state()}.
process_stanza(Stanza, XMPPClient, State) ->
    ping_pong:ping(State#state.ping_pong_pid),
    process_stanza(Stanza, XMPPClient),
    {ok, State}.

%% @private
-spec terminate(term(), state()) -> any().
terminate(pong_timeout, _)                -> ok; %%do nothing
terminate(_, #state{ping_pong_pid = Pid}) -> ping_pong:stop(Pid).

%%====================================================================
%% local functions
%%====================================================================

process_stanza(#xmlel{name = <<"message">>} = Stanza, Client) ->
    case exml_query:attr(Stanza, <<"type">>) of
        <<"chat">> ->
            %% To and From attributes are swapped
            [From, To] = [exml_query:attr(Stanza, Attr) ||
                             Attr <- [<<"to">>, <<"from">>]],
            Text = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
            EchoMsg = escalus_stanza:chat(From, To, Text),
            io:format("EchoMsg = ~p~n", [EchoMsg]),
            escalus:send(Client, EchoMsg);
        _ -> ok
    end;
process_stanza(#xmlel{name = <<"iq">>} = Stanza, Client) ->
    case process_iq(Stanza) of
        ok -> ok;
        Response ->
            escalus:send(Client, Response)
    end;
process_stanza(_Stanza, _Client) ->
    erlang:display(_Stanza),
    ok.


process_iq(Stanza) ->
    %% To and From attributes are swapped
    [Type, Id, From, To] = [exml_query:attr(Stanza, Attr) ||
                               Attr <- [<<"type">>, <<"id">>, <<"to">>, <<"from">>]],
    case Type of
        <<"get">> ->
            case exml_query:subelement(Stanza, <<"ping">>) of
                undefined -> service_unavailable_iq(From, To, Id, Stanza#xmlel.children);
                _ -> iq(From, To, <<"result">>, Id, [])
            end;
        <<"set">> -> service_unavailable_iq(From, To, Id, Stanza#xmlel.children);
        _ -> ok
    end.

iq(From, To, Type, Id, Children) ->
    IQ1 = escalus_stanza:iq(To, Type, Children),
    IQ2 = escalus_stanza:from(IQ1, From),
    escalus_stanza:set_id(IQ2, Id).

service_unavailable_iq(From, To, Id, Children) ->
    Error = escalus_stanza:error_element(<<"cancel">>, <<"service-unavailable">>),
    iq(From, To, <<"error">>, Id, [Error | Children]).