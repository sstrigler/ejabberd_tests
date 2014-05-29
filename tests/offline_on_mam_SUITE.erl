-module(offline_on_mam_SUITE).

%% Using mod_mam as a mod_offline replacement,
%% making sure that (XEP-0280) Message Carbons
%% dont' get archived.
%%
%% NOTICE: This functionality is non-standard

-compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").

-import(escalus_stanza, [set_id/2]).

-define(MAX_WAIT_STANZAS, 10).

all() ->
    [{group, essential}, {group, history}].

groups() ->
    [{essential, [one_message_in_mam,
                  one_exchange_in_mam]},
     {history,  [new_resource_gets_history,
                 deleting_one_user_doesnt_affect_others
                ]}].

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    escalus:create_users(Config).

end_per_group(_, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName,Config) ->
    random:seed(now()),
    escalus:init_per_testcase(CaseName,Config).

end_per_testcase(CaseName,Config) ->
    escalus:end_per_testcase(CaseName,Config).


one_message_in_mam(Config) ->
    given_empty_mam(Config),
    Msg = <<"If thou be in a lonely place, If one hour's calm be thine">>,

    escalus:story(
      Config, [{alice_carbons, 4},{bob, 1}],
      fun(Alice1, Alice2, Alice3, Alice4, Bob) ->
              escalus_client:send(Alice1, chat_w_id(Bob, Msg)),
              all_ok([escalus_client:wait_for_stanza(C) || C <- [Alice2,Alice3,Alice4]]),
              escalus:assert(is_chat_message, [Msg],
                             escalus_client:wait_for_stanza(Bob)),
              [1,1,1,1] = [ length(user_archive(C))
                            || C <- [Alice1,Alice2,Alice3,Alice4] ]
      end).

one_exchange_in_mam(Config) ->
    given_empty_mam(Config),
    Msg1 = <<"If all the earth and all the heaven Now look serene to thee">>,
    Msg2 = <<"If thy love were like mine, how wild Thy longings, even to pain">>,

    escalus:story(
      Config, [{alice_carbons, 3},{bob, 1}],
      fun(Alice1, Alice2, Alice3, Bob) ->
              escalus_client:send(Alice1, chat_w_id(Bob, Msg1)),
              all_ok([escalus_client:wait_for_stanza(C) || C <- [Alice2,Alice3]]),
              escalus:assert(is_chat_message, [Msg1], escalus_client:wait_for_stanza(Bob)),

              escalus_client:send(Bob, chat_w_id(Alice1, Msg2)),
              escalus:assert(is_chat_message, [Msg2], escalus_client:wait_for_stanza(Alice1)),
              all_ok([escalus_client:wait_for_stanza(C) || C <- [Alice2,Alice3]]),

              [2,2,2] = [ length(user_archive(C))
                          || C <- [Alice1,Alice2,Alice3] ]
      end).


new_resource_gets_history(Config) ->
    given_empty_mam(Config),
    AliceSpec = escalus_users:get_userspec(Config, alice_carbons),
    AliceMsgs = [ <<"My love is almost anguish now,">>,
                  <<"It beats so strong and true;">>,
                  <<"'Twere rapture, could I deem that thou">>,
                  <<"Such anguish ever knew.">> ],
    BobMsgs = [
               <<"I have been but thy transient flower,">>,
               <<"Thou wert my god divine;">>,
               <<"Till checked by death's congealing power,">>,
               <<"This heart must throb for thine.">> ],
    escalus:story(
      Config, [{alice_carbons, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
              escalus_client:stop(Alice2),
              [ escalus_client:wait_for_stanza(C) || C <- [Alice1] ],
              %% They've received the presence

              SendF = fun(Amsg,Bmsg) ->
                              escalus_client:send(Alice1, chat_w_id(Bob, Amsg)),
                              escalus_client:send(Bob, chat_w_id(Alice1, Bmsg))
                      end,
              GetF = fun(Amsg,Bmsg) ->
                             escalus:assert(is_chat_message, [Amsg], escalus_client:wait_for_stanza(Bob)),
                             escalus:assert(is_chat_message, [Bmsg], escalus_client:wait_for_stanza(Alice1))
                     end,
              lists:zipwith(SendF, AliceMsgs, BobMsgs),
              lists:zipwith(GetF, AliceMsgs, BobMsgs),

              %% TODO: connect with new resource and collect history
              {ok, Alice2Bis} = escalus_client:start(Config, AliceSpec, <<"res2">>),
              8 = length(user_archive(Alice2Bis)),
              ok
      end).


deleting_one_user_doesnt_affect_others(Config) ->
    given_empty_mam(Config),
    Amsg = <<"Hello there, Bob!">>,
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              escalus_client:send(Alice, chat_w_id(Bob, Amsg)),
              escalus:assert(is_chat_message, [Amsg], escalus_client:wait_for_stanza(Bob)),
              when_user_gets_deleted(Config,bob),
              1 = length(user_archive(Alice)),
              ok
      end).


when_user_gets_deleted(Config, User) ->
    escalus_ejabberd:delete_users(Config, {by_name, [User]}).


given_empty_mam(Config) ->
    lists:map(
      fun([U,S]) -> ok = escalus_ejabberd:rpc(mod_mam, delete_archive, [S,U]) end,
      get_user_jids(Config)).

user_archive(User) ->
    random:seed(now()),
    Qid = list_to_binary(random_alpha_binary(10)),
    escalus_client:send(User, escalus_stanza:mam_archive_query(Qid)),
    Results = escalus_client:wait_for_stanzas(User,?MAX_WAIT_STANZAS),
    filter_archive_results(Qid, Results).

dump_mam(Config) ->
    lists:map(
      fun([U,S]) -> escalus_ejabberd:rpc(mod_mam, archive_size, [S,U]) end,
      get_user_jids(Config)).

%%
%% bookkeeping
%%

all_equal(E, L) ->
    lists:all(fun(X) -> X =:= E end, L).

all_ok(L) ->
    all_equal(ok, L).

chat_w_id(Target, Msg) ->
    Id = random_alpha_binary(10),
    set_id(escalus_stanza:chat_to(Target, Msg), Id).

extract_us({_, Plist}) ->
    [proplists:get_value(username, Plist),
     proplists:get_value(server, Plist)].

filter_archive_results(Qid, Stanzas) ->
    [ S || S <- Stanzas, Qid =:= result_queryid(S) ].

result_queryid(El) ->
    exml_query:path(El, [{element, <<"result">>}, {attr, <<"queryid">>}]).

get_user_jids(Config) ->
    [extract_us(Uspec)
     || Uspec <- escalus_config:get_config(escalus_users, Config)].

random_alpha_binary(Length) ->
    [random:uniform($z - $a + 1) + $a - 1 || _X <- lists:seq(1, Length)].
