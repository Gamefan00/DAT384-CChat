-module(server).
-export([start/1,stop/1]).

% Record to define the structure of the server state
-record(server_state, {server, channel_list, nick_list}).

% For a new server
new_server_state(ServerAtom) ->
    #server_state{
        server = ServerAtom,
        channel_list = [],
        nick_list = []
    }.

% Handles all the requests (later - do several ones for the different kinds of requests)
server_handler(state, response) ->
    not_implemented.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID

    spawn(genserver, start, [ServerAtom, new_server_state(ServerAtom), fun server_handler/2]).
    %not_implemented.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
