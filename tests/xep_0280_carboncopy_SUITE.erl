-module(xep_0280_carboncopy_SUITE).

-compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

all() ->
    [{group, properties},{group, essential}].

groups() ->
    [{essential, [discovering_support,
                  enabling_carbons,
                  disabling_carbons,
                  receiving_messages_to_bare_jid,
                  avoiding_carbons
                 ]},
    {properties, [run_properties]}].

prop_names() ->
    [p_forward_received_messages, p_forward_sent_messages].

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    catch fake_auth_server:start(),
    escalus:create_users(Config).

end_per_group(_, Config) ->
    escalus:delete_users(Config),
    fake_auth_server:stop().

init_per_testcase(CaseName,Config) ->
    escalus:init_per_testcase(CaseName,Config).
end_per_testcase(CaseName,Config) ->
    escalus:end_per_testcase(CaseName,Config).


%%
%%  Properties {group, properties}
%%

run_properties(Config) ->
    Props = proper:conjunction([mk_prop(P, Config) || P <- prop_names()]),
    true = proper:quickcheck(Props, [verbose,long_result, {numtests, 10}]).

mk_prop(PropName, Config) ->
    %% Instantiate a property with a CT config object,
    %% Return a tuple for proper:conjunction to use
    {PropName, apply(?MODULE, PropName, [Config])}.


%%
%%  CT tests {group, essential}
%%
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
    escalus:story(Config, [{alice, 1}], fun carbons_get_enabled/1).

disabling_carbons(Config) ->
    escalus:story(Config, [{alice, 1}],
                  fun(Alice) -> carbons_get_enabled(Alice),
                                carbons_get_disabled(Alice) end).

receiving_messages_to_bare_jid(Config) ->
    escalus:story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1,Alice2,Bob) ->
              carbons_get_enabled([Alice1,Alice2]),
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

avoiding_carbons(Config) ->
    escalus:story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
              carbons_get_enabled([Alice1,Alice2]),
              Msg = escalus_stanza:chat_without_carbon_to(
                      Bob, <<"And pious action">>),
              escalus_client:send(Alice1, Msg),
              escalus:assert(
                is_chat_message, [<<"And pious action">>],
                escalus_client:wait_for_stanza(Bob)),
              escalus_client:wait_for_stanzas(Alice2, 1),
              [] = escalus_client:peek_stanzas(Alice2)
      end).

no_of_resources() -> random:uniform(8).

%%
%% Property generators
%% TODO: Consider moving to separate lib (escalus_prop?)
%%

p_enable_carbons(Config) ->
    ?FORALL(N, no_of_resources(),
            true_story(Config, [{alice, N}], fun carbons_get_enabled/1)).

p_forward_received_messages(Config) ->
    ?FORALL(N, no_of_resources(),
            true_story(Config, [{alice, 1}, {bob, N}],
                       fun all_bobs_secondary_resources_get_forwards/1)).

p_forward_sent_messages(Config) ->
    ?FORALL(N, no_of_resources(),
            true_story(Config, [{alice, 1}, {bob, N}],
                       fun all_bobs_secondary_resources_get_carbons/1)).

%%
%% Test scenarios w/assertions
%%

all_bobs_secondary_resources_get_forwards([Alice,Bob1|Bobs]) ->
    carbons_get_enabled([Bob1|Bobs]),
    escalus_client:send(Alice,
                        escalus_stanza:chat_to(Bob1, <<"'Tis most true:">>)),
    GotForward = fun(BobsResource) ->
                         escalus:assert(
                           is_forwarded_received_message,
                           [escalus_client:full_jid(Alice),
                            escalus_client:full_jid(Bob1),
                            <<"'Tis most true:">>],
                           escalus_client:wait_for_stanza(BobsResource)) end,
    lists:foreach(GotForward, Bobs).

all_bobs_secondary_resources_get_carbons([Alice,Bob1|Bobs]) ->
    carbons_get_enabled([Bob1|Bobs]),
    escalus_client:send(
      Bob1, escalus_stanza:chat_to(Alice, <<"O heavy burden!">>)),
    escalus:assert(
      is_chat_message, [<<"O heavy burden!">>],
      escalus_client:wait_for_stanza(Alice)),
    GotCarbon = fun(BobsResource) ->
                        escalus:assert(
                          is_forwarded_sent_message,
                          [escalus_client:full_jid(Bob1),
                           escalus_client:full_jid(Alice),
                           <<"O heavy burden!">>],
                          escalus_client:wait_for_stanza(BobsResource)) end,
    lists:foreach(GotCarbon, Bobs).

carbons_get_disabled(Client) ->
    IqSet = escalus_stanza:carbons_disable(),
    escalus_client:send(Client, IqSet),
    Result = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_iq, [<<"result">>], Result).

carbons_get_enabled(Clients) when is_list(Clients) ->
    lists:foreach(fun carbons_get_enabled/1, Clients);

carbons_get_enabled(Client) ->
    IqSet = escalus_stanza:carbons_enable(),
    escalus_client:send(Client, IqSet),
    Result = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_iq, [<<"result">>], Result).




%%
%% Internal helpers
%%

%% Wrapper around escalus:story. Returns PropEr result.
true_story(Config, UserSpecs, TestFun) ->
    try   escalus:story(Config, UserSpecs, TestFun), true
    catch E -> {error, E}
    end.
