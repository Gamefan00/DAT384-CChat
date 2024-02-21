-module(server).
-export([start/1,stop/1]).

% Record to define the structure of the server state
-record(server_st, {server, channel_list, nick_list}).

% Record for channels
-record(channel_st, {id, members}).

% For a new server
initial_server_state(ServerAtom) ->
    #server_st{
        server = ServerAtom,
        channel_list = [],
        nick_list = []
    }.

% For a new channel
initial_channel_state(Channel, MemberPid) ->
    #channel_st{
        id = Channel, 
        members = [MemberPid]
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    spawn(genserver, start, [ServerAtom, initial_server_state(ServerAtom), fun server_handler/2]).
    %not_implemented.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.

% Handler for join requests for channel 
channel_handler(Ch_st, {join, Client}) ->
    MembersOfCh = Ch_st#channel_st.members,
    % Handles if the user already has joined or if it should join
    case lists:member(Client, MembersOfCh) of
        true ->
            {reply, {error, user_already_joined, "User already joined"}, Ch_st};
        false ->
            {reply, ok, Ch_st#channel_st{members = MembersOfCh ++ [Client]}}
    end;

% Handles join request
% parameters: the current state of the server, and the request data from client 
server_handler(St, {join, Client, Channel}) ->
    Channels = St#server_st.channel_list, % get all the channels 
    % If the channel already exists, just join the channel. Otherwise
    % start the channel first
    case lists:member(Channel, Channels) of
        true ->
            Response = (catch genserver:request(list_to_atom(Channel), {join, Client})),
            % User successfully joined, so send that as response
            {reply, Response, St};
        false -> 
            spawn(genserver, start, [list_to_atom(Channel), initial_channel_state(Channel, Client), fun channel_handler/2]),
            {reply, ok, St#server_st{channel_list = Channels ++ [Channel]}}
    end.
