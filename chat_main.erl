-module(chat_main).
-export([start/1]).

start([Arg|_]) ->
  P = list_to_integer(Arg),
  io:format("Starting chat server on port ~p~n", [P]),
  spawn_link(fun() -> start(P) end);
start(Port) ->
  D = chat_dispatcher:start(),
  chat_handler:start(D, Port).
