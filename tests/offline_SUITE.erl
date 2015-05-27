%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Suite for testing mod_offline* modules
%%% @end
%%%===================================================================

-module(offline_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, mod_offline_tests}].

all_tests() ->
    [simple_message, timed_racecondition].

groups() ->
    [{mod_offline_tests, [sequence], all_tests()}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    escalus:create_users(Config1, {by_name, [alice, bob]}).

end_per_suite(Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%%===================================================================
%%% offline tests
%%%===================================================================

simple_message(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        escalus:send(Alice, escalus_stanza:chat_to(bob, <<"Hi, Offline!">>))
    end),

    %% Bob logs in
    Bob = login_send_presence(Config, bob),

    %% He receives his initial presence and the message
    Stanzas = escalus:wait_for_stanzas(Bob, 2),
    escalus_new_assert:mix_match([is_presence,
                                  is_chat(<<"Hi, Offline!">>)],
                                 Stanzas),
    escalus_cleaner:clean(Config).

timed_racecondition(Config) ->
    %% Alice sends a message to Bob, who is about to log in.
    escalus:story(
      Config, [{alice, 1}],
      fun(Alice) ->
          %% We vary this scenario over time in order to find the sweet spot.
          lists:foreach(
            fun(I) ->
                    Spec = escalus_users:get_userspec(Config, bob),
                    {ok, Bob} = escalus_client:start(Config, Spec, <<"dummy">>),
                    escalus:send(Bob, escalus_stanza:presence(<<"available">>)),

                    %% Wait a little (varying time) until sending the message.
                    receive
                    after I -> ok
                    end,

                    Message = escalus_stanza:chat_to(bob, <<"Hi, Offline!">>),
                    escalus:send(Alice, Message),

                    %% He receives his initial presence and the message
                    Stanzas = escalus:wait_for_stanzas(Bob, 2),
                    escalus_new_assert:mix_match([is_presence,
                                                  is_chat(<<"Hi, Offline!">>)],
                                                 Stanzas),

                    escalus_client:stop(Bob)
            end,
            %% The incident happens somewhere bettween 70 and 80 msec
            %% - at least on my machine (tm).
            lists:flatten([lists:duplicate(10, El) || El <- lists:seq(70,80)])
           )
      end),

    escalus_cleaner:clean(Config).

%%%===================================================================
%%% Custom predicates
%%%===================================================================

is_chat(Content) ->
    fun(Stanza) -> escalus_pred:is_chat_message(Content, Stanza) end.

%%%===================================================================
%%% Helpers
%%%===================================================================

login_send_presence(Config, User) ->
    Spec = escalus_users:get_userspec(Config, User),
    {ok, Client} = escalus_client:start(Config, Spec, <<"dummy">>),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Client.
