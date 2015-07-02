-module(hello).

-export([auth_on_register/5,
         auth_on_publish/6,
         auth_on_subscribe/3]).

-define(TABLE, ?MODULE).

auth_on_register(Peer, {_, ClientId}, UserName, Password, CleanSession) ->
    error_logger:info_msg("auth_on_register - peer: ~p, client id: ~p, user: ~p, pass: ~p, clean: ~p", [Peer, ClientId, UserName, Password, CleanSession]),
    Table = table(),
    case {UserName, Password} of
        {X, X} ->
            ets:insert(Table, {ClientId, UserName}),
            error_logger:info_msg("registered client ~p by user ~p", [ClientId, UserName]),
            ok;
        _ -> {error, invalid_credentials}
    end.

auth_on_publish(_, {_, ClientId}, QoS, Topic, Payload, IsRetain) ->
    error_logger:info_msg("auth_on_publish - client id: ~p, qos: ~p, topic: ~p, retain: ~p, payload: ~p", [ClientId, QoS, Topic, IsRetain, Payload]),
    Table = table(),
    case ets:lookup(Table, ClientId) of
        [{_, "guest"}] ->
            error_logger:info_msg("guest user cannot publish", []),
            {error, whatever};
        [{_, UserName}] -> 
            error_logger:info_msg("publish user: ~p", [UserName]),
            ok;
        _ ->
            error_logger:info_msg("invalid publish user id: ~p", [ClientId]),
            {error, whatever}
    end.

auth_on_subscribe(_, {_, ClientId}, Topics) ->
    error_logger:info_msg("auth_on_subscribe - user id: ~p, topic: ~p", [ClientId, Topics]),
    Table = table(),
    case ets:lookup(Table, ClientId) of
        [{_, "nosub"}] ->
            error_logger:info_msg("nosub user cannot subscribe", []),
            {error, whatever};
        [{_, UserName}] -> 
            error_logger:info_msg("subscribe user: ~p", [UserName]),
            ok;
        _ ->
            error_logger:info_msg("invalid subscribe user id: ~p", [ClientId]),
            {error, whatever}
    end.

table() ->
    case ets:info(?TABLE, name) of
        ?TABLE ->
            ?TABLE;
        _ ->
            ets:new(?TABLE, [public, named_table, {read_concurrency, true}]),
            ?TABLE
    end.
