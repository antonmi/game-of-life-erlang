-module(gol).
-compile(export_all).
-import(utils, [print_content/1, read_file/1, pprint/2, write_file/3]).

main(Args) ->
  [Steps, Scale] = lists:map(fun erlang:list_to_integer/1, Args),
  [InputFile, OutputFile] = ["data/input.txt", "data/output.txt"],
  D0 = read_file(InputFile),
  file:delete(OutputFile),
  write_file(OutputFile, D0, Scale),
  make_steps(D0, Steps, Scale, OutputFile),
  erlang:halt().

%performs steps
make_steps(D, Count, Scale, OutputFile) when Count > 0 ->
  D1 = get_next_step(D),
  write_file(OutputFile, D1, Scale),
  make_steps(D1, Count - 1, Scale, OutputFile);

make_steps(D, _, _, _) -> D.
%---
%gets next dictionary
get_next_step(D) ->
  D1 = dict:new(),
  get_next_step(D, D1).

get_next_step(D, D1) ->
  Keys = dict:fetch_keys(D),
  NextDic = process_keys(Keys, D, D1),
  NextDic.
%---

process_keys([H|T], D, D1) ->
  Neigh = [H | get_neigh_list(H)],
  DD = process_cells(Neigh, D, D1),
  process_keys(T, D, DD);

process_keys([], _, D1) -> D1.
%---

process_cells([H|T], D, D1) ->
  case dict:is_key(H, D1) of
    true -> process_cells(T, D, D1);
    false ->
      DD = process_cell(H, D, D1),
      process_cells(T, D, DD)
  end;

process_cells([], _, D1) -> D1.
%---

process_cell({X,Y}, D, D1) ->
  case is_alive({X,Y}, D) of
    true ->
      DD = dict:store({X,Y}, 1, D1),
      DD;
    false -> D1
  end.
%---

is_alive({X, Y}, D) ->
  Neigh = get_neigh_list({X,Y}),
  Count = alive_count(Neigh, D),
  case Count of
     2 -> dict:is_key({X, Y}, D);
     3 -> true;
     _ -> false
  end.
%---

alive_count(List, D) ->
  alive_count(List, 0, D).

alive_count([H|T], R, D) ->
  case dict:is_key(H, D) of
    true -> alive_count(T, R+1, D);
    false -> alive_count(T, R, D)
  end;

alive_count([], R, _) -> R.
%---

get_neigh_list({X, Y}) ->
  [{X-1, Y-1}, {X, Y-1}, {X+1, Y-1}, {X-1, Y}, {X+1, Y}, {X-1, Y+1}, {X, Y+1}, {X+1, Y+1} ].
%---
