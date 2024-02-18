-module(server).
-export([start/1,stop/1]).
% Start a new server process with the given name
% Do not change the signature of this function.

start(ServerAtom) ->
    
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    gen_server:start(ServerAtom,initial_state, handle).
    
    -record(server_st, {
        cID, % ID of channel
        server, % Atom of server
        cNames = [] % atom of the chat serve

    }).
initial_state(ServerAtom, CID, CNames) ->
    #server_st{
        server = ServerAtom,
        cID = CID,
        cNames = CNames
    }.

handle_req(St {join, UID, CName, UNick}) ->
    case CName of
        member(CName, )
        
    



% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.