-module(channel).
%-export([start/1,stop/1]).

start(ChannelAtom) ->
    
    gen_server:start(ChannelAtom,initial_state, handle).

    -record(server_st, {
        cID, % ID of channel
        channel, % Atom of server
        uNicks = [] % Current Nicks of active users
    }).