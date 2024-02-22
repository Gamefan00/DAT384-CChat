-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    % TODO: Implement this function
    Response = (catch genserver:request(St#client_st.server, {join, self(), Channel})),
    case Response of
        {'EXIT',_}          -> {reply, {error, server_not_reached, "The server has been stopped"}, St};
        timeout_error       -> {reply, {error, server_not_reached, "Server is unresponsive or has timed out"}, St};
        Other               -> {reply, Response, St}
    end;


% Leave channel
handle(St, {leave, Channel}) ->
    % TODO: Implement this function
    % TODO do we need to handle errors here or are they fixed in server?
    Response = (catch genserver:request(list_to_atom(Channel), {leave, self()})),
    case Response of 
        {'EXIT', _}     -> {reply, {error, user_not_joined, "User is not a participant of that channel"}, St};
        Other           -> {reply, Response, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % TODO: Implement this function
    Response = (catch genserver:request(list_to_atom(Channel), {message_send, Msg, St#client_st.nick, self()})),
    % need to handle if the server has already been shut down
    case Response of 
        {'EXIT', _}     -> {reply, {error, server_not_reached, "Doesn't get any response from server"}, St};
        Other           -> {reply, Response, St}
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
% TODO - started with this one 
handle(St, {nick, NewNick}) ->
    % check if nick is already taken
    Response = (catch genserver:request(St#client_st.server, {nick, NewNick})),
    case Response of
        {error, nick_taken, _Explanation} ->
            {reply, {error, nick_taken, "Nick already taken"}, St};
        Other ->
            {reply, ok, St#client_st{nick = NewNick}}
    end;    

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
