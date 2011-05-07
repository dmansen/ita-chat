-module(tcp_send_test).
-export([connect/2, send/2, stop/1, login/2, join/2, usermsg/3, roommsg/3, logout/1, part/2, stress_test/3]).

connect(Host, Port) ->
  case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
  {ok, Socket} -> Socket;
  Else -> {error, crap}
  end.

stop(Socket) ->
  gen_tcp:close(Socket).

send(Socket, Msg) ->
  ok = gen_tcp:send(Socket, Msg),
  loop().

join_test(Host, Port, Id) ->
  case connect(Host, Port) of
  {error, _} ->
    io:format("Bad news");
  Socket ->
    login(Socket, integer_to_list(Id)),
    join(Socket, "news"),
    forever()
  end.

stress_test(N, Host, Port) ->
  stress_test(N, Host, Port, fun join_test/3).

stress_test(N, Host, Port, F) ->
  lists:map(fun(X) -> spawn(fun() -> F(Host, Port, X) end) end, lists:seq(0, N, 1)).

login(Socket, Name) ->
  ok = gen_tcp:send(Socket, "LOGIN " ++ Name ++ "\r\n"),
  loop().

join(Socket, Room) ->
  ok = gen_tcp:send(Socket, "JOIN #" ++ Room ++ "\r\n"),
  loop().

part(Socket, Room) ->
  ok = gen_tcp:send(Socket, "PART #" ++ Room ++ "\r\n"),
  loop().

usermsg(Socket, User, Msg) ->
  ok = gen_tcp:send(Socket, "MSG " ++ User ++ " " ++ Msg ++ "\r\n"),
  loop().

roommsg(Socket, Room, Msg) ->
  ok = gen_tcp:send(Socket, "MSG #" ++ Room ++ " " ++ Msg ++ "\r\n"),
  loop().

logout(Socket) ->
  ok = gen_tcp:send(Socket, "LOGOUT\r\n"),
  loop().

forever() ->
  receive
    {tcp, _, _} ->
      forever(); 
    X ->
      io:format("~p~n", [X]),
      forever()
  end.

loop() ->
  receive
    X ->
      io:format("~p~n", [X]),
      loop()    
  after 1000 ->
    null
  end.
