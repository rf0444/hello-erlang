-module(hello).

-export([auth_on_register/5,
         auth_on_publish/6,
         auth_on_subscribe/3]).

auth_on_register({_IpAddr, _Port} = Peer, {_MountPoint, _ClientId} = SubscriberId, UserName, Password, CleanSession) ->
    error_logger:info_msg("auth_on_register: ~p ~p ~p ~p ~p", [Peer, SubscriberId, UserName, Password, CleanSession]),
    ok.

auth_on_publish(UserName, {_MountPoint, _ClientId} = SubscriberId, QoS, Topic, Payload, IsRetain) ->
    error_logger:info_msg("auth_on_publish: ~p ~p ~p ~p ~p ~p", [UserName, SubscriberId, QoS, Topic, Payload, IsRetain]),
    ok.

auth_on_subscribe(UserName, ClientId, [{_Topic, _QoS}|_] = Topics) ->
    error_logger:info_msg("auth_on_subscribe: ~p ~p ~p", [UserName, ClientId, Topics]),
    ok.
