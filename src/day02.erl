-module(day02).

-export([ parse_input/1
        , read_input/0
        , run_program/1
        , run_program/2
        , part1/0
        , part2/0
        ]).

-include_lib("eunit/include/eunit.hrl").

%% Main entry ------------------------------------------------------------------

part1() ->
  Program = parse_input(read_input()),
  PatchedProgram = set(2, 2, set(12, 1, Program)),
  run_program(PatchedProgram).

part2() ->
  Program = parse_input(read_input()),
  Combinations = parameter_combinations(),
  [{Noun, Verb}] =
    lists:filter(
      fun(Params) ->
          patch_and_run(Params, Program) =:= 19690720
      end, Combinations),
  100 * Noun + Verb.

%% Logic -----------------------------------------------------------------------

run_program(Program) ->
  {_, Result} = lists:unzip(run_program(Program, 0)),
  Result.

run_program(Program, Position) ->
  case next_op(Program, Position) of
    halt ->
      Program;
    {add, X, Y, Target} ->
      Result = get(X, Program) + get(Y, Program),
      run_program(
        set(Result, Target, Program),
        Position + 4);
    {multiply, X, Y, Target} ->
      Result = get(X, Program) * get(Y, Program),
      run_program(
        set(Result, Target, Program),
        Position + 4)
  end.

next_op([{Position, 1}, {_, X}, {_, Y}, {_, Target} | _], Position) ->
  {add, X, Y, Target};
next_op([{Position, 2}, {_, X}, {_, Y}, {_, Target} | _], Position) ->
  {multiply, X, Y, Target};
next_op([{Position, 99} | _], Position) ->
  halt;
next_op([_ | Rem], Position) ->
  next_op(Rem, Position).

get(Target, Program) ->
  {_, X} = lists:keyfind(Target, 1, Program),
  X.

set(X, Target, Program) ->
  lists:keyreplace(Target, 1, Program, {Target, X}).

patch_and_run({X, Y}, Program) ->
  PatchedProgram = set(X, 1, set(Y, 2, Program)),
  [Result | _] = run_program(PatchedProgram),
  Result.

parameter_combinations() ->
  lists:flatten([[{X, Y} || X <- lists:seq(0, 99)] || Y <- lists:seq(0, 99)]).

%% IO --------------------------------------------------------------------------

parse_input(Input) ->
  lists:zip(lists:seq(0, length(Input) - 1), Input).

read_input() ->
  {ok, Binary} = file:read_file(input_full_path("day02.txt")),
  Input = string:split(string:trim(binary_to_list(Binary)), ",", all),
  lists:map(fun list_to_integer/1, Input).

input_full_path(Filename) ->
  code:priv_dir(aoc19) ++ "/input/" ++ Filename.

%% Tests -----------------------------------------------------------------------

-ifdef(EUNIT).

run_program_test_() ->
  SimpleAddition = parse_input([1, 0, 0, 0, 99]),
  SimpleMultiplication = parse_input([2, 3, 0, 3, 99]),
  Multiplication = parse_input([2, 4, 4, 5, 99, 0]),
  AddAndMult = parse_input([1, 1, 1, 4, 99, 5, 6, 0, 99]),
  ExampleProgram = parse_input([1,9,10,3,2,3,11,0,99,30,40,50]),
  [ ?_assertEqual([2, 0, 0, 0, 99], run_program(SimpleAddition))
  , ?_assertEqual([2, 3, 0, 6, 99], run_program(SimpleMultiplication))
  , ?_assertEqual([2, 4, 4, 5, 99, 9801], run_program(Multiplication))
  , ?_assertEqual([30, 1, 1, 4, 2, 5, 6, 0, 99], run_program(AddAndMult))
  , ?_assertEqual([3500,9,10,70,2,3,11,0,99,30,40,50],
                  run_program(ExampleProgram))
  ].

-endif.
