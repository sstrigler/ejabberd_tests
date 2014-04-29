-module(xep_0280_carboncopy_SUITE).

-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, essential}].


groups() ->
    [{essential, [discovering_support,
                  enabling_carbons,
                  disabling_carbons,
                  receiving_messages_to_bare_jid,
                  receiving_messages_to_full_jid]}].

init_per_suite(Config) -> escalus:init_per_suite(Config).
end_per_suite(Config)  -> escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    fake_auth_server:start(),
    escalus:create_users(Config).
end_per_group(_, Config) ->
    escalus:delete_users(Config),
    fake_auth_server:stop().

init_per_testcase(CaseName,Config) ->
    escalus:init_per_testcase(CaseName,Config).
end_per_testcase(CaseName,Config) ->
    escalus:end_per_testcase(CaseName,Config).

discovering_support(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Alice) ->
              IqGet = escalus_stanza:disco_info(<<"localhost">>),
              escalus_client:send(Alice, IqGet),
              Result = escalus_client:wait_for_stanza(Alice),
              escalus:assert(is_iq_result, [IqGet], Result),
              escalus:assert(has_feature, [<<"urn:xmpp:carbons:2">>], Result)
      end).

enabling_carbons(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun enable_carbons/1).

disabling_carbons(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Alice) ->
              enable_carbons(Alice),
              IqSet = escalus_stanza:carbons_disable(),
              escalus_client:send(Alice, IqSet),
              Result = escalus_client:wait_for_stanza(Alice),
              escalus:assert(is_iq, [<<"result">>], Result)
      end).

receiving_messages_to_bare_jid(Config) ->
    escalus:story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1,Alice2,Bob) ->
              enable_carbons([Alice1,Alice2]),
              escalus_client:send(
                Bob, escalus_stanza:chat_to(escalus_client:short_jid(Alice1),
                                            <<"Most like a gentleman">>)),
              escalus:assert(
                is_chat_message, [<<"Most like a gentleman">>],
                escalus_client:wait_for_stanza(Alice1)),
              escalus:assert(
                is_chat_message, [<<"Most like a gentleman">>],
                escalus_client:wait_for_stanza(Alice2))
      end).

receiving_messages_to_full_jid(Config) ->
    escalus:story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1,Alice2,Bob) ->
              enable_carbons([Alice1,Alice2]),
              escalus_client:send(
                Bob, escalus_stanza:chat_to(Alice1, <<"'Tis most true:">>)),
              escalus:assert(
                is_chat_message, [<<"'Tis most true:">>],
                escalus_client:wait_for_stanza(Alice1)),
              escalus:assert(
                is_forwarded_message, [escalus_client:full_jid(Bob),
                                       escalus_client:full_jid(Alice1),
                                       <<"'Tis most true:">>],
                escalus_client:wait_for_stanza(Alice2))
      end).

%%
%% Internal
%%

enable_carbons(Clients) when is_list(Clients) ->
    lists:foreach(fun enable_carbons/1, Clients);

enable_carbons(Client) ->
    IqSet = escalus_stanza:carbons_enable(),
    escalus_client:send(Client, IqSet),
    Result = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_iq, [<<"result">>], Result).
