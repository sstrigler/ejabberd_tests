-module(xeps_0198_0280_interaction_SUITE).

-compile([export_all]).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
%% -include_lib("proper/include/proper.hrl").

all() ->
    [{group, essential}].

groups() ->
    [{essential, [get_carbons_upon_resume,
                  carbons_remain_enabled]}].


init_per_suite(Config) -> escalus:init_per_suite(Config).
end_per_suite(Config) -> escalus:end_per_suite(Config).

init_per_group(_, Config) -> escalus:create_users(Config).
end_per_group(_, Config) -> escalus:delete_users(Config).

init_per_testcase(CaseName,Config) ->
    escalus_ejabberd:rpc(mnesia, clear_table, [session]),
    escalus_ejabberd:rpc(mnesia, clear_table, [sm_session]),
    escalus_ejabberd:rpc(mnesia, clear_table, [offline_msg]),
    escalus:init_per_testcase(CaseName,Config).

end_per_testcase(CaseName,Config) ->
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
              {_, SMID} = sm_helpers:connect_and_die(Alice2Spec),


              %% First resource has exchange with other user.
              %%
              escalus_client:send(Alice1, escalus_stanza:chat_to(Bob, Txt1)),
              escalus_client:send(Bob, escalus_stanza:chat_to(Alice1, Txt2)),


              %% Second resource resumes connection.
              %%
              Steps = [start_stream, maybe_use_ssl, authenticate,
                       sm_helpers:mk_resume_stream(SMID, 2)],
              {ok, Alice2, _, _} = escalus_connection:start(Alice2Spec, Steps),
              escalus_connection:send(Alice2, escalus_stanza:presence(<<"available">>)),
              _Pres = escalus_connection:get_stanza(Alice2, pres),
              _IqRes = escalus_connection:get_stanza(Alice2, carbon_iq_res),


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
                             FReceived)
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
              {_, SMID} = sm_helpers:connect_and_die(Alice2Spec),

              %% Second resource resumes connection.
              %%
              Steps = [start_stream, maybe_use_ssl, authenticate,
                       sm_helpers:mk_resume_stream(SMID, 2)],
              {ok, Alice2, _, _} = escalus_connection:start(Alice2Spec, Steps),
              escalus_connection:send(Alice2, escalus_stanza:presence(<<"available">>)),

              %% TODO: consider how to hide this book-keeping in escalus_client
              _Pres = escalus_connection:get_stanza(Alice2, pres),
              _IqRes = escalus_connection:get_stanza(Alice2, carbon_iq_res),
              _Pres2 = escalus_connection:get_stanza(Alice2, pres2),
              _R = escalus_connection:get_stanza(Alice2, r),


              %% First resource has exchange with other user.
              %%
              escalus_client:send(Alice1, escalus_stanza:chat_to(Bob, Txt1)),
              escalus_client:send(Bob, escalus_stanza:chat_to(Alice1, Txt2)),


              %% Second resource gets carbons of the exchange.
              %%

              FSent = escalus_connection:get_stanza(Alice2, forwarded_sent),
              _R2 = escalus_connection:get_stanza(Alice2, r2),
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
                             FReceived)
      end).
