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
  Str = make_string(D, Scale),
  io:fwrite(Str).
%---
%write to file
write_file(FileName, D, Scale) ->
  Str = make_string(D, Scale, []),
  file:write_file(FileName, list_to_binary(Str), [append]).
%---
make_string(D, Scale) ->
  make_string(D, Scale, []).

make_string(D, Scale, R) ->
  Matrix = prepare_matrix(Scale),
  make_string(Matrix, D, Scale, R).

make_string([H|T], D, Scale, R) ->
  R1 = make_line(Scale - length(T), H, D, R),
  make_string(T, D, Scale, R1);

make_string([], _, _, R) -> [10 | lists:reverse(R)].
%---

make_line(X, [HY|TY], D, R) ->
  case dict:is_key({HY, X}, D) of
    true ->
      R1 = [35 | R];
    false ->
      R1 = [46 | R]
  end,
  make_line(X, TY, D, R1);

make_line(_, [], _, R) ->
  [10 | R].
%---

prepare_matrix(Scale) ->
  prepare_matrix(lists:seq(1, Scale), Scale, []).

prepare_matrix([_|T], Scale, R) ->
  prepare_matrix(T, Scale, [lists:seq(1, Scale) | R]);

prepare_matrix([], _, R) -> R.
%---

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
  case H == 35 of
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