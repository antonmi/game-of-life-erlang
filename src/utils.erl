-module(utils).
-export([print_content/1, read_file/1, pprint/2]).
-compile(export_all).

%prints dict content (keys and values)
print_content(D) ->
  Keys = dict:fetch_keys(D),
  print_content(Keys, D).

print_content([H|T], D) ->
  print_key_value(H, D),
  print_content(T, D);

print_content([], _) -> ok.
%---
%reads file and prepare initial dict
read_file(FileName) ->
  {ok, Data} = file:read_file(FileName),
  Matrix = make_matrix(parse_data(Data)),
  Dict = make_dict(Matrix),
  Dict.
%---
%prety prints to console
pprint(D, Scale) ->
  Matrix = prepare_matrix(Scale),
  pprint(Matrix, D, Scale).

pprint([H|T], D, Scale) ->
  print_line(Scale - length(T), H, D),
  pprint(T, D, Scale);

pprint([], _, _) -> ok.
%---

print_line(X, [HY|TY], D) ->
  case dict:is_key({HY, X}, D) of
    true ->
      io:fwrite("~p", [1]);
    false ->
      io:fwrite("~p", [0])
  end,
  print_line(X, TY, D);

print_line(_, [], _) ->
  io:fwrite("~n").
%---

prepare_matrix(Scale) ->
  prepare_matrix(lists:seq(1, Scale), Scale, []).

prepare_matrix([_|T], Scale, R) ->
  prepare_matrix(T, Scale, [lists:seq(1, Scale) | R]);

prepare_matrix([], _, R) -> R.
%---

%TODO write file
write_file() ->
  Bin = "..." ++ "\n" ++ "###" ++ "\n" ++ "..." ++ "\n",
  file:write_file("output.txt", Bin).

%private

print_key_value(Key, D) ->
  V = dict:fetch(Key, D),
  io:fwrite("~p => ~p~n", [Key, V]).
%---

parse_data(Data) ->
  ListOfStrings = string:tokens(binary_to_list(Data), "\r\n"),
  ListOfStrings.
%---

make_matrix(ListOfStrings) ->
  make_matrix(ListOfStrings, []).

make_matrix([H|T], R) ->
  make_matrix(T, [make_list(H) | R]);

make_matrix([], R) ->
  lists:reverse(R).
%---

make_list(Str) ->
  make_list(Str, []).

make_list([H|T], R) ->
  case H == 49 of
    true -> make_list(T, [1|R]);
    false -> make_list(T, [0|R])
  end;

make_list([], R) ->
  lists:reverse(R).
%---

make_dict(Matrix) ->
  D = dict:new(),
  make_dict(Matrix, D, 1, 1).

make_dict([H|T], D, X, Y) ->
  case is_list(H) of
    true ->
      D1 = list_to_dict(H, D, X, Y),
      make_dict(T, D1, X, Y+1);
    false -> make_dict(T, D, X, Y+1)
  end;

make_dict([], D, _, _) -> D.
%---

list_to_dict([H|T], D, X, Y) ->
  case H == 1 of
    true ->
      D1 = dict:store({X, Y}, H, D),
      list_to_dict(T, D1, X+1, Y);
    false -> list_to_dict(T, D, X+1, Y)
  end;

list_to_dict([], D, _, _) -> D.
%---