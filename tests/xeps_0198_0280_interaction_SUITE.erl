-module(xeps_0198_0280_interaction_SUITE).

-compile([export_all]).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
%% -include_lib("proper/include/proper.hrl").

all() ->
    [{group, essential}].

groups() ->
    [{essential, [get_carbons_upon_resume]}].


init_per_suite(Config) -> escalus:init_per_suite(Config).
end_per_suite(Config) -> escalus:end_per_suite(Config).

init_per_group(_, Config) -> escalus:create_users(Config).
end_per_group(_, Config) -> escalus:delete_users(Config).

init_per_testcase(CaseName,Config) -> escalus:init_per_testcase(CaseName,Config).
end_per_testcase(CaseName,Config) -> escalus:end_per_testcase(CaseName,Config).



get_carbons_upon_resume(Config) ->

    Txt1 = <<"Crowns him with flowers">>,
    Txt2 = <<"Yes">>,
    ConnSteps = [start_stream, maybe_use_ssl, authenticate, bind, session],
    Alice2Res = <<"res2">>,
    Alice2Spec = [{stream_management, true}, {resource, Alice2Res} |
                  escalus_users:get_options(Config, alice)],


    escalus:story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice1, Bob) ->

          %% Second resource connects w/stream resumption enabled
          %%
          {ok, Alice2, _, _} =
            escalus_connection:start(Alice2Spec, ConnSteps++[stream_resumption]),


          %% Second resource enables carbons and drops connection.
          %%
          carbon_helper:enable_carbons(Alice2),
          escalus_connection:send(Alice2, escalus_stanza:sm_ack(1)),
          kill_connection(Alice2),

          %% First resource has exchange with other user.
          %%
          escalus_client:send(Alice1, escalus_stanza:chat_to(Bob, Txt1)),
          escalus_client:send(Bob, escalus_stanza:chat_to(Alice1, Txt2)),

          %% Second resource resumes connection.
          %%
          {ok, NewAlice2, _, _} = escalus_connection:start(Alice2Spec, ConnSteps),
          escalus_connection:send(NewAlice2, escalus_stanza:presence(<<"available">>)),


          %% Second resource gets carbons of the exchange.
          %%
          FSent = escalus_connection:get_stanza(NewAlice2, forwarded_sent),
          FReceived = escalus_connection:get_stanza(NewAlice2, forwarded_received),

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


%%
%% Internal
%%



kill_connection(#transport{module = escalus_tcp, ssl = SSL,
                                          socket = Socket} = Conn) ->
    %% Ugly, but there's no API for killing the connection
    %% without sending </stream:stream>.
    case SSL of
        true ->
            ssl:close(Socket);
        false ->
            gen_tcp:close(Socket)
    end,
    %% There might be open zlib streams left...
    catch escalus_connection:stop(Conn).
