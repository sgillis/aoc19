-module(day01).

-export([ calculate_fuel_requirement/1
        , parse_input/1
        , part1/0
        , part2/0
        , read_input/0
        , total_fuel_requirement/1
        ]).

-include_lib("eunit/include/eunit.hrl").

%% Main entry ------------------------------------------------------------------

part1() ->
  Input = parse_input(read_input()),
  total_fuel_requirement(Input). %% 3299598

part2() ->
  Input = parse_input(read_input()),
  recursive_total_fuel_requirement(Input). %% 4946546

%% Logic -----------------------------------------------------------------------

total_fuel_requirement(Masses) when is_list(Masses) ->
  lists:sum(lists:map(fun calculate_fuel_requirement/1, Masses)).

calculate_fuel_requirement(Mass) ->
  max(0, Mass div 3 - 2).

recursive_total_fuel_requirement(Masses) when is_list(Masses) ->
  lists:sum(lists:map(fun recursive_fuel_requirement/1, Masses)).

recursive_fuel_requirement(Mass) when Mass =< 0 ->
  0;
recursive_fuel_requirement(Mass) ->
  SimpleFuelRequirement = calculate_fuel_requirement(Mass),
  SimpleFuelRequirement + recursive_fuel_requirement(SimpleFuelRequirement).

%% IO --------------------------------------------------------------------------

parse_input(Input) ->
  lists:map(fun list_to_integer/1, Input).

read_input() ->
  {ok, Binary} = file:read_file(input_full_path("day01.txt")),
  string:split(string:trim(binary_to_list(Binary)), "\n", all).

input_full_path(Filename) ->
  code:priv_dir(aoc19) ++ "/input/" ++ Filename.

-ifdef(EUNIT).

calculate_fuel_requirement_test_() ->
  [ ?_assertEqual(2, calculate_fuel_requirement(12))
  , ?_assertEqual(2, calculate_fuel_requirement(14))
  , ?_assertEqual(654, calculate_fuel_requirement(1969))
  , ?_assertEqual(33583, calculate_fuel_requirement(100756))
  ].

total_fuel_requirement_test_() ->
  ?_assertEqual(34241, total_fuel_requirement([12, 14, 1969, 100756])).

recursive_fuel_requirement_test_() ->
  [ ?_assertEqual(2, calculate_fuel_requirement(12))
  , ?_assertEqual(2, calculate_fuel_requirement(14))
  , ?_assertEqual(966, calculate_fuel_requirement(1969))
  , ?_assertEqual(50346, calculate_fuel_requirement(100756))
  ].

-endif.
