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
        server, % Atom of server
        cNames = [], % Name of all active channels
        uNicks = [] % Current Nicks of active users
    }).
initial_state(ServerAtom, CNames, UNicks) ->
    #server_st{
        server = ServerAtom,
        cNames = CNames,
        uNicks = UNicks
    }.

handle_req(St, {join, UID, CName, UNick}) ->
    
       if 
        member(CName,#server_st.cNames) ->
            

        true ->
            
            %Start a new channel process

       end.
        



% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.

