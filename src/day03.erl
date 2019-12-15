-module(day03).

-export([ part1/0
        , part2/0
        ]).

-include_lib("eunit/include/eunit.hrl").

%% Main entry ------------------------------------------------------------------

part1() ->
  [Line1, Line2] = lists:map(fun parse_segments/1, read_input()),
  minimal_intersection_point(line_to_points(Line1), line_to_points(Line2)).

part2() ->
  [Line1, Line2] = lists:map(fun parse_segments/1, read_input()),
  fastest_intersection_point(line_to_points(Line1), line_to_points(Line2)).

%% Logic -----------------------------------------------------------------------

minimal_intersection_point(Line1, Line2) ->
  Intersections = intersections(Line1, Line2),
  lists:min(lists:map(fun manhattan_distance/1, Intersections)).

fastest_intersection_point(Line1, Line2) ->
  Intersections = intersections(Line1, Line2),
  Speeds =
    lists:map(fun(Point) ->
                  find_steps_to(Line1, Point) + find_steps_to(Line2, Point)
              end, Intersections),
  lists:min(Speeds).

find_steps_to(Line, Point) ->
  {Point, Length} =
    lists:keyfind(
      Point, 1, lists:zip(Line, lists:seq(1, length(Line)))),
  Length.

line_to_points(Segments) ->
  [_Origin | Line] = lists:reverse(line_to_points(Segments, [{0,0}])),
  Line.

line_to_points([], Points) ->
  Points;
line_to_points([Segment | Segments], Points = [LastPoint | _Rem]) ->
  NewSegment = lists:reverse(segment_to_points(LastPoint, Segment)),
  NewPoints = lists:flatten([NewSegment, Points]),
  line_to_points(Segments, NewPoints).

segment_to_points({OX, OY}, {right, Xs}) ->
  [{OX + X, OY} || X <- lists:seq(1, Xs)];
segment_to_points({OX, OY}, {left, Xs}) ->
  [{OX - X, OY} || X <- lists:seq(1, Xs)];
segment_to_points({OX, OY}, {up, Ys}) ->
  [{OX, OY + Y} || Y <- lists:seq(1, Ys)];
segment_to_points({OX, OY}, {down, Ys}) ->
  [{OX, OY - Y} || Y <- lists:seq(1, Ys)].

intersections(PointLine1, PointLine2) ->
  sets:to_list(sets:intersection(
                 sets:from_list(PointLine1),
                 sets:from_list(PointLine2)
                )).

manhattan_distance({X, Y}) ->
  abs(X) + abs(Y).

%% IO --------------------------------------------------------------------------

parse_segments(RawSegments) ->
  lists:map(fun parse_segment/1, RawSegments).

parse_segment([$R | Number]) ->
  {right, list_to_integer(Number)};
parse_segment([$L | Number]) ->
  {left, list_to_integer(Number)};
parse_segment([$U | Number]) ->
  {up, list_to_integer(Number)};
parse_segment([$D | Number]) ->
  {down, list_to_integer(Number)}.

read_input() ->
  {ok, Binary} = file:read_file(input_full_path("day03.txt")),
  Lines = string:split(string:trim(binary_to_list(Binary)), "\n", all),
  lists:map(fun(Line) -> string:split(Line, ",", all) end, Lines).

input_full_path(Filename) ->
  code:priv_dir(aoc19) ++ "/input/" ++ Filename.

%% Tests -----------------------------------------------------------------------

-ifdef(EUNIT).

test_line(1) ->
  string:split("R8,U5,L5,D3", ",", all);
test_line(2) ->
  string:split("U7,R6,D4,L4", ",", all);
test_line(3) ->
  string:split("R75,D30,R83,U83,L12,D49,R71,U7,L72", ",", all);
test_line(4) ->
  string:split("U62,R66,U55,R34,D71,R55,D58,R83", ",", all);
test_line(5) ->
  string:split("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", ",", all);
test_line(6) ->
  string:split("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", ",", all).

parse_segments_test_() ->
  ?_assertEqual([ {right, 8}
                , {up, 5}
                , {left, 5}
                , {down, 3}
                ], parse_segments(test_line(1))).

line_to_points_test_() ->
  [ ?_assertEqual([{1,0}, {2,0}], line_to_points(parse_segments(["R2"])))
  , ?_assertEqual([ {1,0}, {2,0}, {3,0}, {4,0}, {5,0}, {6,0}, {7,0}, {8,0}
                  , {8,1}, {8,2}, {8,3}, {8,4}, {8,5}
                  , {7,5}, {6,5}, {5,5}, {4,5}, {3,5}
                  , {3,4}, {3,3}, {3,2}
                  ], line_to_points(parse_segments(test_line(1))))
  ].

intersections_test_() ->
  Line1 = line_to_points(parse_segments(test_line(1))),
  Line2 = line_to_points(parse_segments(test_line(2))),
  [ ?_assertEqual([{6,5}, {3,3}], intersections(Line1, Line2)) ].

minimal_intersection_point_test_() ->
  Line1 = line_to_points(parse_segments(test_line(1))),
  Line2 = line_to_points(parse_segments(test_line(2))),
  Line3 = line_to_points(parse_segments(test_line(3))),
  Line4 = line_to_points(parse_segments(test_line(4))),
  Line5 = line_to_points(parse_segments(test_line(5))),
  Line6 = line_to_points(parse_segments(test_line(6))),
  [ ?_assertEqual(6, minimal_intersection_point(Line1, Line2))
  , ?_assertEqual(159, minimal_intersection_point(Line3, Line4))
  , ?_assertEqual(135, minimal_intersection_point(Line5, Line6))
  ].

fastest_intersection_point_test_() ->
  Line1 = line_to_points(parse_segments(test_line(1))),
  Line2 = line_to_points(parse_segments(test_line(2))),
  Line3 = line_to_points(parse_segments(test_line(3))),
  Line4 = line_to_points(parse_segments(test_line(4))),
  Line5 = line_to_points(parse_segments(test_line(5))),
  Line6 = line_to_points(parse_segments(test_line(6))),
  [ ?_assertEqual(30, fastest_intersection_point(Line1, Line2))
  , ?_assertEqual(610, fastest_intersection_point(Line3, Line4))
  , ?_assertEqual(410, fastest_intersection_point(Line5, Line6))
  ].

-endif.
