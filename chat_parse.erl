-module(chat_parse).
-export([split_command/1, extract_tokens/1]).

split_command(String) ->
  split_by_list(String, "\r\n").

split_by_list(List, Split) ->
  split_by_list(List, Split, []).

split_by_list([], _, _) ->
  nomatch;
split_by_list(List = [H|T], Split, Accum) ->
  case lists:prefix(Split, List) of
  true ->
    {lists:reverse(Accum), lists:nthtail(length(Split), List)};
  false ->
    split_by_list(T, Split, [H|Accum])
  end.

extract_tokens(Text) ->
  {First, Rest} = lists:splitwith(fun(X) -> X /= $\s end, Text),
  case Rest of
    [$\s|End] ->
      {First, End};
    [] ->
      {Id, _} = split_command(First),
      {Id, "\r\n"}
  end.
