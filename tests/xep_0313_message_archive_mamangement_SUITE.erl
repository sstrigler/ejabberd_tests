-module(xep_0313_message_archive_mamangement_SUITE).

%% This module, in contrast to mam_SUITE, depends on
%% mod_mam being configured on the running server under test.

-compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").

-import(escalus_stanza, [set_id/2]).

-define(MAX_WAIT_STANZAS, 10).

all() ->
    [{group, essential}, {group, history}, {group, queries}].

groups() ->
    [{essential, [one_message_in_mam,
                  one_exchange_in_mam]},
     {history, [new_resource_gets_history,
                deleting_one_user_doesnt_affect_others]},
     {queries, [lookup_with_jid,
                lookup_with_start_date,
                lookup_with_end_date,
                lookup_with_message_id,
                lookup_with_paging,
                lookup_with_paging_backwards
               ]}
    ].

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
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              given_exchange(Alice, Bob, <<"Greetings, Earthling!">>),

              [ArchivedMsg] = user_archive(Alice),
              escalus:assert(is_mam_archived_message, [<<"Greetings, Earthling!">>], ArchivedMsg),

              when_user_gets_deleted(Config,bob),
              [ArchivedMsg_] = user_archive(Alice),
              escalus:assert(is_mam_archived_message, [<<"Greetings, Earthling!">>], ArchivedMsg_)
      end).

lookup_with_jid(Config) ->
    given_empty_mam(Config),
    escalus:story(
      Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
              given_exchange(Alice, Bob, <<"Hello there, William">>),
              given_exchange(Alice, Carol, <<"One potato">>),
              given_exchange(Alice, Carol, <<"Two potato">>),


              2 = length(user_archive_with_jid(Alice, <<"carol@localhost">>)),
              1 = length(user_archive_with_jid(Alice, <<"bob@localhost">>))
      end).

lookup_with_start_date(Config) ->
    given_empty_mam(Config),
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice,Bob) ->
              given_exchange(Alice, Bob, <<"Stale message">>),
              given_elapsed_time(),

              PivotTime = iso8601:format(now()),

              given_exchange(Alice, Bob, <<"Fresh message">>),

              [ArchivedMsg] = user_archive_with_start(Alice, PivotTime),
              escalus:assert(is_mam_archived_message, [<<"Fresh message">>], ArchivedMsg)
      end).

lookup_with_end_date(Config) ->
    given_empty_mam(Config),
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice,Bob) ->
              given_exchange(Alice, Bob, <<"Early message">>),
              given_elapsed_time(),

              PivotTime = iso8601:format(now()),
              given_exchange(Alice, Bob, <<"Late message">>),

              [ArchivedMsg] = user_archive_with_end(Alice, PivotTime),
              escalus:assert(is_mam_archived_message, [<<"Early message">>], ArchivedMsg)
      end).

lookup_with_message_id(Config) ->
    given_empty_mam(Config),
    MsgID = <<"999-888-666">>,
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice,Bob) ->
              given_exchange(Alice, Bob, <<"Unimportant message">>),
              given_elapsed_time(),

              given_exchange(Alice, Bob, <<"An important message">>, MsgID),
              given_exchange(Alice, Bob, <<"Another important message">>),

              [ArchiveMsg] =  user_archive_with_message_id(Alice, MsgID),
              escalus:assert(is_mam_archived_message, [<<"Another important message">>], ArchiveMsg)
      end).

lookup_with_paging(Config) ->
    given_empty_mam(Config),

    %% Message 100 will be the freshest
    Ms = lists:map(fun(X) -> B = integer_to_binary(X), <<"Message ", B/binary>> end,
                   lists:seq(1,100)),
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice,Bob) ->
              [ given_exchange(Alice,Bob,M) || M <- Ms ],

              ArchivePart1 = user_archive_in_full(Alice),
              50 = length(ArchivePart1),
              LastId = result_id(last(ArchivePart1)),
              ArchivePart2 = user_archive_with_message_id(Alice, LastId),
              50 = length(ArchivePart2),
              escalus:assert(is_mam_archived_message,
                             [<<"Message 100">>],
                             last(ArchivePart2))
      end).

lookup_with_paging_backwards(Config) ->
    given_empty_mam(Config),

    %% Message 100 will be the Freshest
    {LastM,LastID} = {<<"Message 100">>, <<"777-888-999-000">>},
    Ms = lists:map(fun(X) -> B = integer_to_binary(X), <<"Message ", B/binary>> end,
                                 lists:seq(1,99)),
    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice,Bob) ->
              [ given_exchange(Alice,Bob,M) || M <- Ms ],
              given_exchange(Alice,Bob,LastM,LastID),

              ArchivePart1 = user_archive_with_message_id_backwards(Alice, LastID),
              50 = length(ArchivePart1),

              MiddleId = result_id(last(ArchivePart1)),
              ArchivePart2 = user_archive_with_message_id_backwards(Alice, MiddleId),

              49 = length(ArchivePart2),
              escalus:assert(is_mam_archived_message,
                              [<<"Message 1">>],
                              last(ArchivePart2)), %% The oldest message comes in last
              FirstId = result_id(last(ArchivePart2)),

              EndOfArchive = user_archive_with_message_id_backwards(Alice, FirstId),
              0 = length(EndOfArchive)
      end).


%%
%% Testing functions
%%
given_empty_mam(Config) ->
    lists:map(
      fun([U,S]) -> ok = escalus_ejabberd:rpc(mod_mam, delete_archive, [S,U]) end,
      get_user_jids(Config)).

given_exchange(From, To, MsgText) ->
    given_exchange(From, To, MsgText, random_alpha_binary(10)).

given_exchange(From, To, MsgText, MsgId) ->
    escalus_client:send(From, chat_w_id(To, MsgText, MsgId)),
    escalus:assert(is_chat_message, [MsgText], escalus_client:wait_for_stanza(To)).

given_elapsed_time() ->
    %% Using timer:sleep is cheating, but I don't see a better way to do this without going straight
    %% into the database.
    timer:sleep(1001).

when_user_gets_deleted(Config, User) ->
    escalus_ejabberd:delete_users(Config, {by_name, [User]}).

user_archive_with_jid(User, WithJID) ->
    get_result(
      User,
      fun(Qid) -> escalus_stanza:mam_lookup_messages_iq(Qid,undefined,undefined,WithJID) end).

user_archive_with_start(User, StartTimestamp) ->
    get_result(
      User,
      fun(Qid) -> escalus_stanza:mam_lookup_messages_iq(Qid,StartTimestamp,undefined,undefined) end).

user_archive_with_end(User, EndTimestamp) ->
    get_result(
      User,
      fun(Qid) -> escalus_stanza:mam_lookup_messages_iq(Qid,undefined,EndTimestamp,undefined) end).

user_archive_with_message_id(User, MsgID) ->
    get_result(
      User,
      fun(Qid) -> escalus_stanza:mam_lookup_messages_iq(Qid,undefined,undefined,undefined,{'after',MsgID}) end,
      105
     ).

user_archive_with_message_id_backwards(User, MsgID) ->
    get_result(
      User,
      fun(Qid) -> escalus_stanza:mam_lookup_messages_iq(Qid,undefined,undefined,undefined,{'before',MsgID}) end,
      105
     ).

user_archive(User) ->
    get_result(User, fun(Qid) -> escalus_stanza:mam_archive_query(Qid) end).

user_archive_in_full(User) ->
    get_result(User, fun(Qid) -> escalus_stanza:mam_archive_query(Qid) end, 105).

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

%% TODO move these two into escalus
chat_w_id(Target, Msg) ->
    Id = random_alpha_binary(10),
    set_id(escalus_stanza:chat_to(Target, Msg), Id).
chat_w_id(Target, Msg, Id) ->
    set_id(escalus_stanza:chat_to(Target, Msg), Id).

extract_us({_, Plist}) ->
    [proplists:get_value(username, Plist),
     proplists:get_value(server, Plist)].

filter_archive_results(Qid, Stanzas) ->
    [ S || S <- Stanzas, Qid =:= result_queryid(S) ].

get_result(User,StanzaConstructor,MaxStanzas) ->
    Qid = list_to_binary(random_alpha_binary(10)),
    Payload = StanzaConstructor(Qid),
    escalus_client:send(User, Payload),
    Results = escalus_client:wait_for_stanzas(User,MaxStanzas),
    filter_archive_results(Qid, Results).

%% The constructor fun will get the query id
get_result(User,StanzaConstructor) ->
    get_result(User,StanzaConstructor,?MAX_WAIT_STANZAS).

get_user_jids(Config) ->
    [extract_us(Uspec)
     || Uspec <- escalus_config:get_config(escalus_users, Config)].

last(L) -> hd(lists:reverse(L)).

now_us({MegaSecs,Secs,MicroSecs}) ->
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.


result_queryid(El) ->
    exml_query:path(El, [{element, <<"result">>}, {attr, <<"queryid">>}]).

result_id(El) ->
    exml_query:path(El, [{element, <<"result">>}, {attr, <<"id">>}]).

random_alpha_binary(Length) ->
    [random:uniform($z - $a + 1) + $a - 1 || _X <- lists:seq(1, Length)].
