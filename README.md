Mercutio [![CI](https://github.com/esl/Mercutio/workflows/CI/badge.svg)](https://github.com/esl/Mercutio/actions?query=workflow%3ACI)
---

A demo [XMPP component](http://xmpp.org/extensions/xep-0114.html) based on [escalus](https://github.com/esl/escalus).

It supports ping IQ and echos all the messages back.

Build
---

```
rebar3 release
```

Run
---

To run mercutio we need to start XMPP server/cluster first.

```
docker network create mim_cluster

docker run --rm -d -t -h mongooseim-1 --name mongooseim-1 \
           -v `pwd`/ci_testing/config_files/ejabberd.cfg:/member/ejabberd.cfg \
           --network=mim_cluster -p 5222:5222 -p 8888:8888 \
           mongooseim/mongooseim:3.1.1

docker run --rm -d -t -h mongooseim-2 --name mongooseim-2 \
           -v `pwd`/ci_testing/config_files/ejabberd.cfg:/member/ejabberd.cfg \
           --network=mim_cluster -p 8889:8888 \
           mongooseim/mongooseim:3.1.1
```

after that we need to set environment variable required for successful connection

```
export MIM_HOSTS="127.0.0.1;localhost:8889"
export PASSWORD="secret"
export XMPP_DOMAIN="localhost"
export DOMAIN_PREFIX="some.component.subdomain"
export PING_TIMEOUT="3600000"
```

then we can start mercutio

```
_build/default/rel/mercutio/bin/mercutio
```

Test
----
don't forget to add some new users if you want to test mercutio component manually using some external XMPP client software
```
docker exec -it mongooseim-1 /usr/lib/mongooseim/bin/mongooseimctl register bob localhost makrolika
docker exec -it mongooseim-1 /usr/lib/mongooseim/bin/mongooseimctl register alice localhost makota
```
another option is to use escalus for client connections, you can run the next commands just in mercutio shell:

1. register some new users (skip this step if you have done it using `docker exec ...` commands):

```
%% register Alice & Bob
escalus_users:create_users([], escalus:get_users([alice, bob])).
```

2. connect some user:

```
EchoComponent = <<"some.component.subdomain.localhost">>,
User1 = <<"user1@", EchoComponent/bitstring>>,
%% connect Alice & Bob
[{alice, ACfg}, {bob, BCfg}] = escalus:get_users([alice, bob]),
{ok, Alice, _Features} = escalus_connection:start(ACfg),
{ok, Bob, _} = escalus_connection:start(BCfg), ok.
```

3. check that mercutio is properly registered under `some.component.subdomain.localhost` domain:

```
%% discover connected services
Discovery = escalus_stanza:service_discovery(<<"localhost">>),
escalus:send(Alice, Discovery),
escalus:wait_for_stanza(Alice).

```

4. try to send some message to mercutio component and check that mercutio echos it back:

```
%% send some messages to service
Msg1 = escalus_stanza:chat_to(EchoComponent, <<"Hi!">>),
Msg2 = escalus_stanza:chat_to(User1, <<"Hi!">>),
escalus:send_and_wait(Alice, Msg1).
escalus:send_and_wait(Alice, Msg2).
```

5. try to send ping IQ to mercutio:

```
PingStanza1=escalus_stanza:ping_request(EchoComponent),
PingStanza2=escalus_stanza:ping_request(User1),
escalus:send_iq_and_wait_for_result(Alice, PingStanza1).
escalus:send_iq_and_wait_for_result(Alice, PingStanza2).
```

6. try to send some not supported IQ to mercutio:

```
Discovery1 = escalus_stanza:service_discovery(EchoComponent),
Discovery2 = escalus_stanza:service_discovery(User1),
escalus:send_and_wait(Alice, Discovery1).
escalus:send_and_wait(Alice, Discovery2).
```

7. try to send out some message from the component, try to pretend another user:

```
Msg3 = escalus_stanza:chat_to(Alice, <<"Hi!">>).
Msg4 = escalus_stanza:from(Msg3, EchoComponent).
Msg5 = escalus_stanza:from(Msg3, User1).
Msg6 = escalus_stanza:from(Msg3, Bob).
[echo_component:send_stanza(M) || M <- [Msg3, Msg4, Msg5, Msg6]].
escalus:wait_for_stanzas(Alice, 100, 0).
```

8. try to send out some IQ from the component:

```
NewIQ = fun(IQ,From,To) ->
          IQ1 = escalus_stanza:set_id(IQ,escalus_stanza:id()),
          IQ2 = case From of
                  undefined -> IQ1;
                  _ -> escalus_stanza:from(IQ1, From)
                end,
          IQ3 = case To of
                  undefined -> IQ2;
                  _ -> escalus_stanza:to(IQ2, To)
                end,
           IQ3
        end.
{ok, PingEl} = exml:parse(<<"<ping xmlns='urn:xmpp:ping'/>">>).

%IQ stanza w/o 'from' or 'to' attribute
Ping = escalus_stanza:iq(<<"get">>, [PingEl]).
escalus:send_iq_and_wait_for_result(Alice, Ping).
echo_component:send_stanza(Ping).

%IQ stanza w/o 'from' attribute
PingWithToAttr=NewIQ(Ping, undefined, <<"localhost">>).
echo_component:send_stanza(PingWithToAttr).

%IQ stanza w/o 'to' attribute
PingWithFromAttr1=NewIQ(Ping, EchoComponent, undefined).
echo_component:send_stanza(PingWithFromAttr1).
PingWithFromAttr2=NewIQ(Ping, User1, undefined).
echo_component:send_stanza(PingWithFromAttr2).

%IQ stanza with 'from' and 'to' attributes
PingWithAllAttributes1=NewIQ(Ping, EchoComponent, <<"localhost">>).
echo_component:send_stanza(PingWithAllAttributes1).
PingWithAllAttributes2=NewIQ(Ping, User1, <<"localhost">>).
echo_component:send_stanza(PingWithAllAttributes2).
```





