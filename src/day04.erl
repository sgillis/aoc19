-module(day04).

-export([ part1/0
        , part2/0
        , satisfies_extended_rules/1
        ]).

-include_lib("eunit/include/eunit.hrl").

%% Main entry ------------------------------------------------------------------

part1() ->
  Range = lists:seq(min_value(), max_value()),
  length(lists:filter(fun satisfies_rules/1, Range)).

part2() ->
  Range = lists:seq(min_value(), max_value()),
  length(lists:filter(fun satisfies_extended_rules/1, Range)).

min_value() ->
  136760.

max_value() ->
  595730.

%% Logic -----------------------------------------------------------------------

satisfies_rules(Number) ->
  Rules = [ fun adjacent_digits/1
          , fun increasing_digits/1
          ],
  NumberString = integer_to_list(Number),
  lists:all(fun(X) -> X end, [Rule(NumberString) || Rule <- Rules]).

satisfies_extended_rules(Number) ->
  Rules = [ fun increasing_digits/1
          , fun pair_of_adjacent_digits/1
          ],
  NumberString = integer_to_list(Number),
  lists:all(fun(X) -> X end, [Rule(NumberString) || Rule <- Rules]).

adjacent_digits([]) ->
  false;
adjacent_digits([_X]) ->
  false;
adjacent_digits([_X, _X | _Rem]) ->
  true;
adjacent_digits([_X | Rem]) ->
  adjacent_digits(Rem).

increasing_digits([]) ->
  true;
increasing_digits([_X]) ->
  true;
increasing_digits([X, Y | Rem]) when X =< Y ->
  increasing_digits([Y | Rem]);
increasing_digits(_) ->
  false.

pair_of_adjacent_digits([]) ->
  false;
pair_of_adjacent_digits([_]) ->
  false;
pair_of_adjacent_digits([X, X]) ->
  true;
pair_of_adjacent_digits([X, X, Y | _Rem]) when X =/= Y ->
  true;
pair_of_adjacent_digits([X, X, X | Rem]) ->
  pair_of_adjacent_digits(lists:dropwhile(fun(Elem) -> Elem =:= X end, Rem));
pair_of_adjacent_digits([_ | Rem]) ->
  pair_of_adjacent_digits(Rem).

%% Tests -----------------------------------------------------------------------

-ifdef(EUNIT).

satisfies_rules_test_() ->
  [ ?_assert(satisfies_rules(111111))
  , ?_assert(not satisfies_rules(223450))
  , ?_assert(not satisfies_rules(123789))
  ].

satisfies_extended_rules_test_() ->
  [ ?_assert(satisfies_extended_rules(112233))
  , ?_assert(not satisfies_extended_rules(123444))
  , ?_assert(satisfies_extended_rules(111122))
  ].

-endif.
