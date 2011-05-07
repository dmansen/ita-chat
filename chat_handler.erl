-module(chat_handler).
-export([start/2]).

-define(MAX_MSG_LENGTH, 1000).
-define(MAX_NAME_LENGTH, 30).
-define(MAX_ROOM_LENGTH, 30).
-define(MAX_CMD_LENGTH, 8).

%% Starts the server on port Port, with process Dispatcher as the
%% control process
start(Dispatcher, Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0},
                                               {reuseaddr, true},
                                               {active, true}]),
                               spawn(fun() -> listen(Dispatcher, Listen, Port) end).

%% Listen thread. This spawns a new process for each
%% new connection that gets made.
%%
%% Dispatcher: the master process as defined by master
listen(Dispatcher, Listen, Port) ->

  case gen_tcp:accept(Listen) of
  {ok, Socket} ->
    io:format("Socket ~p opened~n", [Socket]),

    %% Spawn the function that waits for another
    %% connection
    spawn(fun() -> listen(Dispatcher, Listen, Port) end),

    %% Enter the loop that waits for login
    login_loop(Socket, Dispatcher, <<>>);

  {error, closed} ->
    %% If there was an error, simply recurse on
    %% this function and continue waiting.
    gen_tcp:close(Listen),
    {ok, L} = gen_tcp:listen(Port, [binary, {packet, 0},
                                            {reuseaddr, true},
                                            {active, true}]),
 
    listen(Dispatcher, L, Port);

  {error, _Reason} ->
    listen(Dispatcher, Listen, Port)
  end.

%% process_command is a function that pattern
%% matches on a single command from TCP.

%% Processes a login command. Checks to make sure the
%% username is not too long, and that it does not
%% contain spaces.
process_command(<<"LOGIN ", Rest/binary>>) ->

  %% Grab the desired name using split_command
  {NewName, _} = chat_parse:split_command(binary_to_list(Rest)),
  %% Boolean, tests if we have a space
  Spaces = lists:member($\s, NewName),

  if
  length(NewName) > ?MAX_NAME_LENGTH ->
    Len = list_to_binary(integer_to_list(?MAX_NAME_LENGTH)),
    {error, <<"Names cannot exceed ", Len/binary, " characters">>};
  Spaces ->
    {error, <<"Names cannot contain spaces">>};
  true ->
    {login, list_to_binary(NewName)}
  end;

%% Processes a room join command. 
process_command(<<"JOIN #", Rest/binary>>) ->

  %% Grab the room using split_command
  {Room, _} = chat_parse:split_command(binary_to_list(Rest)),

  if 
  length(Room) > ?MAX_ROOM_LENGTH ->
    Len = list_to_binary(integer_to_list(?MAX_ROOM_LENGTH)),
    {error, <<"Room names cannot exceed ", Len/binary, " characters">>};
  true ->
    case lists:member($\s, Room) of
    false ->
      {join, list_to_binary(Room)};
    true ->
      {error, <<"Room names cannot contain spaces">>}
    end
  end;

%% Process a room part command.
process_command(<<"PART #", Rest/binary>>) ->

  {Room, _} = chat_parse:split_command(binary_to_list(Rest)),

  if 
  length(Room) > ?MAX_ROOM_LENGTH ->
    Len = list_to_binary(integer_to_list(?MAX_ROOM_LENGTH)),
    {error, <<"Room names cannot exceed ", Len, " characters">>};
  true ->
    case lists:member($\s, Room) of
    false ->
      {part, list_to_binary(Room)};
    true ->
      {error, <<"Room names cannot contain spaces">>}
    end
  end;

%% Processes a user-to-room message.
process_command(<<"MSG #", Rest/binary>>) ->

  %% Grab the room and the message
  {Room, Msg} = chat_parse:extract_tokens(binary_to_list(Rest)),

  if
  length(Msg) < ?MAX_MSG_LENGTH + 2 ->
    {roommsg, list_to_binary(Room), 
      list_to_binary(Msg)};
  true ->
    Len = list_to_binary(integer_to_list(?MAX_MSG_LENGTH)),
    {error, <<"Messages cannot exceed ", Len/binary, " characters">>}
  end;

%% Processes a user-to-user message.
process_command(<<"MSG ", Rest/binary>>) ->

  {User, Msg} = chat_parse:extract_tokens(binary_to_list(Rest)),

  if
  length(Msg) < ?MAX_MSG_LENGTH ->
    {usermsg, list_to_binary(User), 
      list_to_binary(Msg)};
  true ->
    Len = list_to_binary(integer_to_list(?MAX_MSG_LENGTH)),
    {error, <<"Messages cannot exceed ", Len/binary, " characters">>}
  end;

process_command(<<"LOGOUT\r\n", _Rest/binary>>) ->
  logout;

process_command(_Else) ->
  {error, <<"Invalid command">>}.

%% wait_for_more takes a (possible) command and a list of strings
%% that contain possible commands. It checks to see if the command
%% has a valid prefix (and thus we should continue waiting), or if
%% we should throw away the input.
wait_for_more(Cmd, CmdList) ->

  StringCmd = binary_to_list(Cmd),
  PrefixList = lists:dropwhile(fun(X) -> 
    not lists:prefix(X, StringCmd) end, CmdList),
  MaxLength = ?MAX_MSG_LENGTH + ?MAX_CMD_LENGTH,

  case PrefixList of
  [_|_] ->
    if
    length(StringCmd) < MaxLength -> 
      true; 
    true -> 
      {error, <<"Command exceeds 1000 characters">>}
    end;

  [] ->
    if 
    length(StringCmd) < ?MAX_CMD_LENGTH ->
      true;
    true -> 
      {error, <<"Invalid command">>}
    end
  end.

%% login_loop is the receive loop, when we are waiting for the
%% login.
login_loop(Socket, Dispatcher, SoFar) ->

  %% Use parse_command to pull off one line
  case parse_command(SoFar) of

  {{error, _Reason} = E, Rest} ->
    notify_user(Socket, E),
    login_loop(Socket, Dispatcher, Rest);

  {logout, _} ->
    gen_tcp:close(Socket),
    io:format("Socket ~p closed~n", [Socket]);

  %% If they tried to log in, we send a message to the dispatcher
  %% trying to log in, then wait for the response.
  {{login, _Name} = M, Rest} ->

    notify_master(Dispatcher, M),
    receive
    {Dispatcher, ok} ->
      notify_user(Socket, ok),
      loop(Socket, Dispatcher, Rest);
    {Dispatcher, Error} ->
      notify_user(Socket, Error),
      login_loop(Socket, Dispatcher, Rest)
    end;

  %% If we get here, that means they tried to use a valid
  %% command without being logged in
  {_Else, Rest} ->
    notify_user(Socket, {error, <<"No login">>}),
    login_loop(Socket, Dispatcher, Rest);

  %% If we reach the false case, we don't yet have a full
  %% line from TCP.
  false ->
    io:format("Waiting for more (login)~n"), 
    case wait_for_more(SoFar, ["LOGIN "]) of

    %% If we should wait, then continue assembling the line
    %% with the next message
    true ->
      receive
      {tcp, Socket, Bin} ->
         Rcvd = <<SoFar/binary, Bin/binary>>,
         login_loop(Socket, Dispatcher, Rcvd);

      {tcp_closed, Socket} ->
         io:format("Socket ~p closed ~n", [Socket])
      end;

    %% Otherwise, wait_for_more gave us an error.
    Error -> 
      notify_user(Socket, Error),
      login_loop(Socket, Dispatcher, <<>>)
    end
 end.
      
%% Main message loop for when the user has been logged
%% in.
loop(Socket, Dispatcher, SoFar) ->
  case parse_command(SoFar) of

  {{login, _}, Rest} ->
    notify_user(Socket, {error, <<"Already logged in">>}),
    loop(Socket, Dispatcher, Rest);

  {Cmd, Rest} ->
    dispatch(Socket, Dispatcher, Cmd),
    loop(Socket, Dispatcher, Rest);  

  false ->
    io:format("waiting for message (loop)~n"),
    case wait_for_more(SoFar, command_list()) of
    true ->
      receive
      {tcp, Socket, Bin} ->
        Rcvd = <<SoFar/binary, Bin/binary>>,
        loop(Socket, Dispatcher, Rcvd);

      {tcp_closed, Socket} ->
        notify_master(Dispatcher, logout),
        io:format("Socket ~p closed~n", [Socket]);

      {Dispatcher, logout} ->
        gen_tcp:close(Socket),
        io:format("Socket ~p closed~n", [Socket]);

      {Dispatcher, Message} ->
        notify_user(Socket, Message),
        loop(Socket, Dispatcher, SoFar)
      end;

    Error ->
      notify_user(Socket, Error),
      loop(Socket, Dispatcher, <<>>)
    end
  end.

parse_command(Commands) ->
  case chat_parse:split_command(binary_to_list(Commands)) of
    nomatch ->
      false;
    {Cmd, Rest} ->
      {process_command(<<(list_to_binary(Cmd))/binary, "\r\n">>), 
        list_to_binary(Rest)}
  end.

%% Dispatches a message to the user or the dispatcher,
%% depending on the pattern.
dispatch(Socket, _, {error, _Reason} = E) ->
  notify_user(Socket, E);

%% Otherwise, we are sending a message to the
%% dispatcher first.
dispatch(_, Dispatcher, Cmd) ->
  notify_master(Dispatcher, Cmd).

%% Notifies the client of an event. Converts tuples to the
%% appropriate TCP string. The tuple formats are as follows:
%%
%% ok : Indicates their command went through
%% {error, Reason} : Indicates there was an error
%% {usermsg, Name, Msg} : Indicates that the user received a
%%                        direct message from Name
%% {roommsg, Name, Room, Msg} : Indicates that the user received
%%                              a message from Name broadcast to
%%                              room Room
notify_user(Socket, ok) ->
  gen_tcp:send(Socket, <<"OK\r\n">>);
notify_user(Socket, {error, Reason}) ->
  gen_tcp:send(Socket, <<"ERROR ", Reason/binary, "\r\n">>);
notify_user(Socket, {roommsg, Name, Room, Msg}) ->
  gen_tcp:send(Socket, <<"GOTROOMMSG ", Name/binary, " #", Room/binary,
      " ", Msg/binary>>);
notify_user(Socket, {usermsg, Name, Msg}) ->
  gen_tcp:send(Socket, <<"GOTUSERMSG ", Name/binary, " ",
    Msg/binary>>).

notify_master(Pid, Msg) ->
  Pid ! {self(), Msg}.

command_list() ->
  ["JOIN #", "PART #", "MSG #", "MSG ", "LOGOUT\r\n"].
