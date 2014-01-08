-module(gol).
-compile(export_all).
-import(utils, [print_content/1, read_file/1, pprint/2]).

main() ->
  D0 = read_file("../data/init_map.txt"),
  pprint(D0, 12),
  D1 = make_steps(D0, 10),
  pprint(D1, 12),
  erlang:halt().

make_steps(D, Count) when Count > 0 ->
  D1 = get_next_step(D),
  pprint(D1, 12),
  io:fwrite("-----~n"),
  make_steps(D1, Count - 1);

make_steps(D, _) -> D.

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
  Neigh = get_neigh_list(H),
  DD = process_cells(Neigh, D, D1),
  process_keys(T, D, DD);

process_keys([], _, D1) ->
  io:fwrite("~n"),
  D1.
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
  io:fwrite("."),
  case is_alive({X,Y}, D) of
    true ->
      DD = dict:store({X,Y}, 1, D1),
      DD;
    false ->
      D1
  end.
%---

is_alive({X, Y}, D) ->
  All = get_neigh_list({X,Y}),
  Neigh = lists:filter(fun({X1,Y1}) -> not((X1==X) and (Y1==Y)) end, All),
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
  get_neigh_list({X, Y}, [-1,0,1], [-1,0,1], []).

get_neigh_list({X,Y}, [HX|TX], [HY|TY], R) ->
  get_neigh_list({X,Y}, TX, [HY|TY], [{X+HX, Y+HY} | R]);

get_neigh_list({X, Y}, [], [_|TY], R) ->
  get_neigh_list({X,Y}, [-1,0,1], TY, R);

get_neigh_list(_, _, [], R) -> R.
%---
