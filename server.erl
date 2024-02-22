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

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    genserver:request(ServerAtom, close_channels),
    genserver:stop(ServerAtom).

%%%%%%%%%%%%%%%% channel handlers %%%%%%%%%%%%%%%%%%%%%%%%%
% Several functions to handle the channels' requests. 
% Parameters are Ch_st, which is the current state of the channel, and the request data
% Returns a tuple {reply, DataSent, ChNewState}. What is sent to the Client/Server
% is what is in the DataSent. The updated state for the channel is ChNewState

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

% Handler for leaving a channel
channel_handler(Ch_st, {leave, Client}) ->
    Members = Ch_st#channel_st.members,
    % Checks if the user is actually in the channel, ie a member.
    % If it is a member it should be removed, and otherwise not
    case lists:member(Client, Members) of
        true ->
            {reply, ok, Ch_st#channel_st{members = lists:delete(Client, Members)}};
        false -> 
            {reply, {error, user_not_joined, "The user is not a member of this channel"}, Ch_st}
     end;

% Handler for message requests
channel_handler(Ch_st, {message_send, Message, Nick, SenderPid}) ->
    ChannelMembers = Ch_st#channel_st.members,
    Channel = Ch_st#channel_st.id,
    % You can only send a msg if you are a member of the channel, 
    % so here membership is checked. If you are a member, all the other
    % members in the channel should get your msg. Otherwise error handling is done
    case lists:member(SenderPid, ChannelMembers) of
        true ->
            [spawn(genserver, request, [Receiver, {message_receive, Channel, Nick, Message}])
                || Receiver <- ChannelMembers, Receiver =/= SenderPid],
            {reply, ok, Ch_st};
        false ->
            {reply, {error, user_not_joined, "The user is not a member of the channel"}, Ch_st}
    end.


%%%%%%%%%%%%%%% server handlers %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions for handling the server requests. These requests are from the clients
% The parameters are St, which is the current state of the server, and request data
% Returns a tuple {reply, DataSent, ServerNewState}. What is sent to the Client
% is what is in the DataSent. The updated state for the server is ServerNewState

% Handles join request
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
    end;

% handles shut down request, and stops all processes for the channels
server_handler(St, close_channels) ->
    lists:foreach(fun(Channel) -> genserver:stop(list_to_atom(Channel)) end, St#server_st.channel_list),
    {reply, ok, St#server_st{channel_list = []}};

% Handles request to change nick (note! only started the distinction assignment, didn't get it to work)
server_handler(St, {nick, NewNick}) ->
    case is_nick_taken(St, NewNick) of
        true ->
            {reply, {error, nick_taken, "Nick already taken"}, St};
        false ->
            {reply, ok, St}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%% Helper method %%%%%%%%%%%%%%%%%%%%
% checks if the nick is already taken
is_nick_taken(St, NewNick) ->
    lists:member(NewNick, St#server_st.nick_list).