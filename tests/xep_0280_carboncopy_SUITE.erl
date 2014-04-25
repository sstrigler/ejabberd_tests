-module(xep_0280_carboncopy_SUITE).

-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, essential}].

groups() ->
    [{essential, [], [server_returns_carbons_capability,
                      server_enables_carbons,
                      server_disables_carbons,
                      second_resource_is_ccd
                     ]}].

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

server_returns_carbons_capability(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Alice) ->
              IqGet = escalus_stanza:disco_info(<<"localhost">>),
              escalus_client:send(Alice, IqGet),
              Result = escalus_client:wait_for_stanza(Alice),
              
              escalus:assert(is_iq_result, [IqGet], Result),
              escalus:assert(has_feature, [<<"urn:xmpp:carbons:2">>], Result)
      end).

server_enables_carbons(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun enable_carbons/1).

server_disables_carbons(Config) ->
    escalus:story(
      Config, [{alice, 1}],
      fun(Alice) ->
              enable_carbons(Alice),
              IqSet = escalus_stanza:carbons_disable(),
              escalus_client:send(Alice, IqSet),
              Result = escalus_client:wait_for_stanza(Alice),
              escalus:assert(is_iq, [<<"result">>], Result)
      end).

second_resource_is_ccd(Config) ->
    escalus:story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1,Alice2,Bob) ->
              enable_carbons(Alice2),
              escalus_client:send(Alice1,
                                  escalus_stanza:chat_to(Bob, <<"Hi!">>)),
              escalus_client:wait_for_stanza(Alice2)
      end).

%%
%% Internal
%%

enable_carbons(Client) ->
    IqSet = escalus_stanza:carbons_enable(),
    escalus_client:send(Client, IqSet),
    Result = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_iq, [<<"result">>], Result).
