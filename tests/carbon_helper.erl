-module(carbon_helper).
-include_lib("escalus/include/escalus.hrl").
-export([enable_carbons/1, disable_carbons/1]).

enable_carbons(Clients) when is_list(Clients) ->
    lists:foreach(fun enable_carbons/1, Clients);
enable_carbons(#transport{} = T) ->
    escalus_connection:send(T, escalus_stanza:carbons_enable()),
    Result = escalus_connection:get_stanza(T, carbon_iq_response),
    escalus:assert(is_iq, [<<"result">>], Result);
enable_carbons(Client) ->
    IqSet = escalus_stanza:carbons_enable(),
    escalus_client:send(Client, IqSet),
    Result = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_iq, [<<"result">>], Result).


disable_carbons(Clients) when is_list(Clients) ->
    lists:foreach(fun disable_carbons/1, Clients);
disable_carbons(Client) ->
    IqSet = escalus_stanza:carbons_disable(),
    escalus_client:send(Client, IqSet),
    Result = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_iq, [<<"result">>], Result).
