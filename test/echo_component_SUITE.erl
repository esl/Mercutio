%%%-------------------------------------------------------------------
%%% @author denys.gonchar@erlang-solutions.com
%%% @copyright 2018 Erlang Solutions Ltd.
%%% @doc
%%%    basic test suite for echo component.
%%% @end
%%%-------------------------------------------------------------------

-module(echo_component_SUITE).

%% general suite interfaces
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([disco_request/1,
         ping_request/1,
         echo_message/1]).

-include_lib("escalus/include/escalus_xmlns.hrl").

-define(ECHO_COMPONENT_DOMAIN, "echo.localhost").
-define(ECHO_COMPONENT, <<?ECHO_COMPONENT_DOMAIN>>).
-define(BOB, <<"bob@", ?ECHO_COMPONENT_DOMAIN>>).

all() ->
    [disco_request,
     ping_request,
     echo_message].

groups() ->
    [].

%%====================================================================
%% init/end functions
%%====================================================================
init_per_suite(Config) ->
    escalus:init_per_suite(Config),
    escalus:create_users(Config),
    Config.

end_per_suite(Config) ->
    escalus:delete_users(Config),
    escalus:end_per_suite(Config),
    ok.


init_per_testcase(TestCase, Config) ->
    escalus:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    escalus:end_per_testcase(TestCase, Config).

%%====================================================================
%% test cases
%%====================================================================
echo_message(Config) ->
    escalus:story(Config,
                  [{alice, 1}],
                  fun(Alice) ->
                      test_echo_msg(Alice,?ECHO_COMPONENT, <<"This is some test text">>),
                      test_echo_msg(Alice,?BOB,<<"This is another test text">>)
                  end).

disco_request(Config) ->
    escalus:story(Config,
                  [{alice, 1}],
                  fun(Alice) ->
                      Server = escalus_client:server(Alice),
                      Discovery = escalus_stanza:service_discovery(Server),
                      Response = escalus:send_and_wait(Alice, Discovery),
                      escalus:assert(is_iq_result, [Discovery], Response),
                      escalus:assert(has_item,[?ECHO_COMPONENT],Response)
                  end).

ping_request(Config) ->
    escalus:story(Config,
                  [{alice, 1}],
                  fun(Alice) ->
                      test_ping_iq(Alice, ?ECHO_COMPONENT),
                      test_ping_iq(Alice, ?BOB)
                  end).

%%====================================================================
%% internal functions
%%====================================================================
test_ping_iq(From, To) ->
    Ping = escalus_stanza:ping_request(To),
    Response = escalus:send_and_wait(From, Ping),
    escalus:assert(is_iq_result, [Ping], Response),
    escalus_assert:is_stanza_from(To, Response).

test_echo_msg(From,To,Text)->
    Msg = escalus_stanza:chat_to(To, Text),
    Response = escalus:send_and_wait(From, Msg),
    escalus_assert:is_chat_message(Text, Response),
    escalus_assert:is_stanza_from(To, Response).