-module(chat_dispatcher).
-export([main/1, start/0]).

%% Does all of the initialization for the app.

main(Port) ->
  D = self(),
  spawn(fun() -> chat_handler:start(D, Port) end),
  start().

%% Creates a new master server. This creates two ets tables,
%% one for rooms on the server and one for users. The structure
%% of the tables is as follows:
%%
%% Rooms table: {Room (binary), pid}, where each room can have multiple
%%              entries for the different users in the room
%% Users table: {pid, Name (binary)}, each entry is unique
%%
%% The process id returned should be passed to all parallel
%% handlers that are associated with the server.
start() ->
  spawn(fun() -> loop(ets:new(users, [set, private]), 
                                  ets:new(rooms, [bag, private])) end).

%% process_msg is the function that handles each
%% type of message. Pattern matching is employed
%% for each message. It takes care of handling the
%% message, checking for errors, and dispatching
%% the correct messages to the user process(es).
process_msg({From, {login, Name}}, Users, _Rooms) ->

  %% Spec that matches any records that have the same name
  %% as the desired username.
  Spec = [{{'_', '$1'}, [{'=:=', '$1', {const, Name}}], ['$1']}],

  case ets:select(Users, Spec) of
  %% If there exists a user with the same name, signal an
  %% error
  [_|_] ->
    msg_client(From, {error, <<"Username already exists">>});

  %% Otherwise, add to the table.
  [] ->
    msg_client(From, ok),
    ets:insert(Users, {From, Name})
  end;

%% Adds a user to a room. Note that if the user is already
%% in the room, this still reports OK.
process_msg({From, {join, Room}}, _Users, Rooms) ->
  ets:insert(Rooms, {Room, From}),
  msg_client(From, ok);

%% Sends a message to all users in a room.
process_msg({From, {roommsg, Room, Msg}}, Users, Rooms) ->

  %% This query finds all users that are in the room,
  %% excluding the user that sent the message.
  Spec = [{{'$1', '$2'}, [{'=:=', '$1', {const, Room}}, 
          {'=/=', '$2', {const, From}}], ['$2']}],
  Recips = ets:select(Rooms, Spec),

  %% This looks up the user that sent the message
  %% (so we can have their username).
  [{From, Name}|_] = ets:lookup(Users, From),

  %% For each user in the list of recipients, send
  %% their process the message.
  lists:map(fun(X) ->
    msg_client(X, {roommsg, Name, Room, Msg}) end, Recips),

  %% Notify the client that it went through.
  msg_client(From, ok);

%% Sends a message from one user to another.
process_msg({From, {usermsg, Recipient, Msg}}, Users, _Rooms) ->

  %% This query finds the pid of the recipient.
  Spec = [{{'$1', '$2'},[{'=:=', '$2', {const, Recipient}}], ['$1']}],

  %% This lookup gets the user that sent the message.
  [{From, Name}|_] = ets:lookup(Users, From),
  Matches = ets:select(Users, Spec),

  %% Check to see if we found a match
  case Matches of
  [Pid|_] ->
    %% Dispatch the message
    msg_client(Pid, {usermsg, Name, Msg}),
    %% Notify of success
    msg_client(From, ok);
  [] ->
    %% Notify of an error
    msg_client(From, {error, <<"User does not exist">>})
  end;
 
%% Removes a user from a room
process_msg({From, {part, Room}}, _Users, Rooms) ->

  %% Query matches the record of the user in the specified room
  Spec = [{{'$1', '$2'}, [{'=:=', '$1', {const, Room}}, 
                          {'=:=', '$2', {const, From}}], [true]}],
  ets:select_delete(Rooms, Spec),
  msg_client(From, ok);

%% Logout action. Removes the user from the Users table
%% and removes any room records associated with that user.
process_msg({From, logout}, Users, Rooms) ->
  
  %% Query matches all room records of that user
  Spec = [{{'_', '$1'}, [{'=:=', '$1', {const, From}}], [true]}],
  ets:select_delete(Rooms, Spec),
  ets:delete(Users, From),
  msg_client(From, logout).
 
%% Main message receive loop. Dispatches to process_msg
loop(Users, Rooms) ->
  receive
  M ->
    process_msg(M, Users, Rooms),
    loop(Users, Rooms)
  end.

%% Sends a message to a client process, attaching the
%% master pid along with it.
msg_client(Pid, Msg) ->
  Pid ! {self(), Msg}.
