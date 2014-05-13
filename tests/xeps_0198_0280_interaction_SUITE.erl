-module(xeps_0198_0280_interaction_SUITE).

-compile([export_all]).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, essential}].

groups() ->
    [{essential, [get_carbons_upon_resume,
                  carbons_remain_enabled,
                  resource_addressed_carbons_get_misdelivered]}].

init_per_suite(Config) -> escalus:init_per_suite(Config).
end_per_suite(Config) -> escalus:end_per_suite(Config).

init_per_group(_, Config) -> escalus:create_users(Config).
end_per_group(_, Config) -> escalus:delete_users(Config).

init_per_testcase(CaseName,Config) ->
    clear_mnesia(),
    escalus:init_per_testcase(CaseName,Config).

end_per_testcase(CaseName,Config) ->
    clear_mnesia(),
    escalus:end_per_testcase(CaseName,Config).

get_carbons_upon_resume(Config) ->
    Txt1 = <<"Msg-1">>,
    Txt2 = <<"Msg-2">>,

    Alice2Res = <<"res2">>,
    Alice2Spec = [{stream_management, true}, {resource, Alice2Res} |
                  escalus_users:get_options(Config, alice)],

    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice1, Bob) ->
              %% Second resource enables carbons and drops connection
              {_, SMID, LastH} = sm_helpers:connect_and_die(Alice2Spec),

              %% First resource has exchange with other user.
              %%
              escalus_client:send(Alice1, escalus_stanza:chat_to(Bob, Txt1)),
              escalus_client:send(Bob, escalus_stanza:chat_to(Alice1, Txt2)),

              %% Second resource resumes connection.
              %%
              Steps = [start_stream, maybe_use_ssl, authenticate,
                       sm_helpers:mk_resume_stream(SMID, LastH)],
              {ok, Alice2, _, _} = escalus_connection:start(Alice2Spec, Steps),
              escalus_connection:send(Alice2, escalus_stanza:presence(<<"available">>)),

              %% Second resource gets carbons of the exchange.
              %%
              FSent = escalus_connection:get_stanza(Alice2, forwarded_sent),
              FReceived = escalus_connection:get_stanza(Alice2, forwarded_received),

              escalus:assert(is_forwarded_sent_message,
                             [escalus_client:full_jid(Alice1),
                              escalus_client:full_jid(Bob),
                              Txt1],
                             FSent),

              escalus:assert(is_forwarded_received_message,
                             [escalus_client:full_jid(Bob),
                              escalus_client:full_jid(Alice1),
                              Txt2],
                             FReceived),
              sm_helpers:discard_offline_messages(Config, alice)
      end).

carbons_remain_enabled(Config) ->
    Txt1 = <<"Msg-11">>,
    Txt2 = <<"Msg-22">>,

    Alice2Res = <<"res2">>,
    Alice2Spec = [{stream_management, true}, {resource, Alice2Res} |
                  escalus_users:get_options(Config, alice)],

    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice1, Bob) ->

              %% Second resource enables carbons and drops connection
              {_, SMID, LastH} = sm_helpers:connect_and_die(Alice2Spec),

              %% Second resource resumes connection.
              %%
              Steps = [start_stream, maybe_use_ssl, authenticate,
                       sm_helpers:mk_resume_stream(SMID, LastH)],
              {ok, Alice2, _, _} = escalus_connection:start(Alice2Spec, Steps),
              escalus_connection:send(Alice2, escalus_stanza:presence(<<"available">>)),
              _Pres = escalus_connection:get_stanza(Alice2, presence),

              %% First resource has exchange with other user.
              %%
              escalus_client:send(Alice1, escalus_stanza:chat_to(Bob, Txt1)),
              escalus_client:send(Bob, escalus_stanza:chat_to(Alice1, Txt2)),


              %% Second resource gets carbons of the exchange.
              %%

              FSent = escalus_connection:get_stanza(Alice2, forwarded_sent),
              FReceived = escalus_connection:get_stanza(Alice2, forwarded_received),

              escalus:assert(is_forwarded_sent_message,
                             [escalus_client:full_jid(Alice1),
                              escalus_client:full_jid(Bob),
                              Txt1],
                             FSent),

              escalus:assert(is_forwarded_received_message,
                             [escalus_client:full_jid(Bob),
                              escalus_client:full_jid(Alice1),
                              Txt2],
                             FReceived),
              sm_helpers:discard_offline_messages(Config, alice)
      end).

%% This test asserts *unwanted behaviour*.
%% Success means that mod_offline resends carbon copies to any resource,
%% not the actually intended recipient resource.
resource_addressed_carbons_get_misdelivered(Config) ->
    Txt1 = <<"Msg-1">>,

    AliceSpec = escalus_users:get_options(Config, alice),
    Alice2Res = <<"res2">>,
    Alice2Spec = [{stream_management, true}, {resource, Alice2Res} | AliceSpec],

    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice1, Bob) ->
        %% Alice2 enables carbons and drops connection;
        %% her process is waiting for stream resumption.
        sm_helpers:connect_and_die(Alice2Spec),

        %% Alice1 receives presence pushes from Alice2.
        [P1, P2] = escalus:wait_for_stanzas(Alice1, 2),
        escalus:assert(is_presence, P1),
        escalus:assert(is_stanza_from, [spec_to_jid(Alice2Spec)], P1),
        escalus:assert(is_presence, P2),
        escalus:assert(is_stanza_from, [spec_to_jid(Alice2Spec)], P2),

        %% Bob talks to Alice1;
        %% Alice2 receives carbons.
        escalus:send(Bob, escalus_stanza:chat_to(Alice1, Txt1)),
        escalus:assert(is_chat_message, [Txt1],
                       escalus:wait_for_stanza(Alice1)),

        %% Send original sender and receiver JIDs to self so that
        %% we can later use them in an assertion...
        self() ! {alice, escalus_client:full_jid(Alice1)},
        self() ! {bob, escalus_client:full_jid(Bob)}
    end),
    %% ...and receive them at once.
    {bob, OriginalSender} = receive {bob, _} = BobJid -> BobJid
                            after 0 -> undefined end,
    {alice, OriginalRecipient} = receive {alice, _} = AliceJid -> AliceJid
                                 after 0 -> undefined end,

    %% Alice1 and Bob disconnect cleanly.
    %% We ensure Alice2's lingering process exits so that her carbons
    %% hit offline storage.
    stop_c2s_waiting_for_resumption(Alice2Spec, Alice2Res),
    0 = length(user_resources(Config, alice)),
    0 = length(user_resources(Config, bob)),

    %% Alice1 connects again;
    %% she should not receive carbons intended for Alice2,
    %% but due to mod_offline being resource-unaware SHE DOES!!!11
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    OfflineCarbon = escalus_connection:get_stanza(Alice, offline_carbon),
    escalus:assert(is_forwarded_received_message,
                   [OriginalSender, OriginalRecipient, Txt1],
                   OfflineCarbon),
    PresencePush = escalus_connection:get_stanza(Alice, presence_push),
    escalus:assert(is_presence_with_type, [<<"available">>], PresencePush).

%%
%% Internal
%%

clear_mnesia() ->
    escalus_ejabberd:rpc(mnesia, clear_table, [session]),
    escalus_ejabberd:rpc(mnesia, clear_table, [sm_session]).

user_resources(Config, User) when is_atom(User) ->
    Spec = escalus_users:get_options(Config, User),
    user_resources(Config, Spec);
user_resources(_Config, Spec) ->
    U = proplists:get_value(username, Spec),
    S = proplists:get_value(server, Spec),
    escalus_ejabberd:rpc(ejabberd_sm, get_user_resources, [U, S]).

stop_c2s_waiting_for_resumption(Spec, Res) ->
    {ok, C2SPid} = sm_helpers:get_session_pid(Spec, Res),
    MRef = erlang:monitor(process, C2SPid),
    C2SPid ! resume_timeout,
    receive {'DOWN', MRef, _, _, _} -> ok
    after 1000 -> ct:fail("timeout waiting for c2s to stop") end.

spec_to_jid(Spec) ->
    U = proplists:get_value(username, Spec),
    S = proplists:get_value(server, Spec),
    R = proplists:get_value(resource, Spec),
    <<U/bytes, "@", S/bytes, "/", R/bytes>>.
