%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Unit test for wms_state module
%%% @end
%%% Created : 08. May 2019 08:05
%%%-------------------------------------------------------------------
-module(wms_state_test).
-author("Attila Makra").

-include_lib("eunit/include/eunit.hrl").

-define(CHK(Exp, Literal, Op),
  ?assertEqual(Exp,
               wms_common:eval_operation(Literal, Op))).

-define(CHK(Exp, Literal1, Op, Literal2),
  ?assertEqual(Exp,
               wms_common:eval_operation(Literal1, Op, Literal2))).

eval_op_test() ->
  test_eval_op_one_arg(),
  test_eval_op_two_arg().

test_eval_op_one_arg() ->
  ?CHK(x, x, nop),

  ?CHK(true, false, '!'),
  ?CHK(false, true, '!'),
  ?CHK({error, {'!', 12}}, 12, '!'),

  ?CHK({e1, []}, [e1], {'--', head}),
  ?CHK({e1, [e2]}, [e1, e2], {'--', head}),
  ?CHK({error, {{'--', head}, {[]}}}, [], {'--', head}),

  ?CHK({e1, []}, [e1], {'--', tail}),
  ?CHK({e2, [e1]}, [e1, e2], {'--', tail}),
  ?CHK({error, {{'--', tail}, {[]}}}, [], {'--', tail}),

  ?CHK({e1, [e1]}, [e1], {'?', head}),
  ?CHK({e1, [e1, e2]}, [e1, e2], {'?', head}),
  ?CHK({error, {{'?', head}, {[]}}}, [], {'?', head}),

  ?CHK({e1, [e1]}, [e1], {'?', tail}),
  ?CHK({e2, [e1, e2]}, [e1, e2], {'?', tail}),
  ?CHK({error, {{'?', tail}, {[]}}}, [], {'?', tail}),

  ?CHK({error, {{'?', tail}, no_list}}, no_list, {'?', tail}).


test_eval_op_two_arg() ->
  test_eval_op_two_base(),
  test_eval_op_two_date(),
  test_eval_op_two_time(),
  test_eval_op_two_list(),
  ok.

test_eval_op_two_base() ->
  % plus operator
  ?CHK("abcd", "ab", '+', "cd"),
  ?CHK(20 + 30, 20, '+', 30),
  ?CHK(20.1 + 30.2, 20.1, '+', 30.2),
  ?CHK(true, true, '+', true),
  ?CHK(true, true, '+', false),
  ?CHK(true, false, '+', true),
  ?CHK(false, false, '+', false),
  ?CHK(<<"abcd">>, <<"ab">>, '+', <<"cd">>),
  ?CHK({error, {'+', {x, y}}}, x, '+', y),

  % minus operator
  ?CHK("ab", "abcd", '-', "cd"),
  ?CHK(20 - 30, 20, '-', 30),
  ?CHK(20.1 - 30.2, 20.1, '-', 30.2),
  ?CHK(true, true, '-', false),
  ?CHK(false, true, '-', true),
  ?CHK(true, false, '-', true),
  ?CHK(false, false, '-', false),
  ?CHK({error, {'-', {x, y}}}, x, '-', y),

  % multiple operator
  ?CHK("Bc", "aBc", '*', "cBd"),
  ?CHK(20 * 30, 20, '*', 30),
  ?CHK(20.1 * 30.2, 20.1, '*', 30.2),
  ?CHK(true, true, '*', true),
  ?CHK(false, true, '*', false),
  ?CHK(false, false, '*', true),
  ?CHK(false, false, '*', false),
  ?CHK({error, {'*', {x, y}}}, x, '*', y),

  % divisor operator
  ?CHK(20 div 30, 20, '/', 30),
  ?CHK(20.1 / 30.2, 20.1, '/', 30.2),
  ?CHK({error, {'/', {x, y}}}, x, '/', y),

  % invalid operator
  ?CHK({error, {invalid, eval_operation, '?', 10}}, 10, '?'),

  ok.

test_eval_op_two_date() ->

  % date mod plus
  ?CHK({2000, 1, 2}, {1999, 12, 31}, {'+', day}, 2),
  ?CHK({2000, 2, 29}, {1999, 12, 31}, {'+', month}, 2),
  ?CHK({2001, 2, 28}, {2000, 12, 31}, {'+', month}, 2),
  ?CHK({2001, 12, 31}, {1999, 12, 31}, {'+', year}, 2),
  ?CHK({error, {{'+', year}, {{1999, 12, 31}, "apple"}}},
       {1999, 12, 31}, {'+', year}, "apple"),
  ?CHK({error, {{'+', year}, {"abc", 2}}}, "abc", {'+', year}, 2),


  % date mod minus
  ?CHK({1999, 12, 31}, {2000, 1, 2}, {'-', day}, 2),
  ?CHK({1999, 11, 30}, {2000, 1, 31}, {'-', month}, 2),
  ?CHK({2000, 12, 28}, {2001, 2, 28}, {'-', month}, 2),
  ?CHK({1999, 12, 31}, {2001, 12, 31}, {'-', year}, 2),
  ?CHK({error, {invalid, eval_operation, {'-', xx}, {{2001, 12, 31}, 2}}},
       {2001, 12, 31}, {'-', xx}, 2),
  ?CHK({error, {{'-', year}, {{2001, 12, 31}, 2.2}}},
       {2001, 12, 31}, {'-', year}, 2.2),
  ?CHK({error, {{'-', year}, {{2001, 12, 31}, "apple"}}},
       {2001, 12, 31}, {'-', year}, "apple"),

  % date set
  ?CHK({2000, 2, 29}, {2000, 2, 1}, {set, 'day'}, 'end'),
  ?CHK({2000, 2, 11}, {2000, 2, 1}, {set, 'day'}, 11),
  ?CHK({error, {{set, day}, {{2000, 2, 1}, "apple"}}},
       {2000, 2, 1}, {set, 'day'}, "apple"),
  ?CHK({error, {{set, day}, {"abc", 11}}}, "abc", {set, 'day'}, 11),

  ?CHK({2000, 12, 29}, {2000, 2, 29}, {set, 'month'}, 'end'),
  ?CHK({2000, 11, 30}, {2000, 1, 30}, {set, 'month'}, 11),
  ?CHK({error, {invalid_date, {2000, 11, 31}}},
       {2000, 1, 31}, {set, 'month'}, 11),
  ?CHK({error, {{set, day}, {{2000, 2, 1}, "apple"}}},
       {2000, 2, 1}, {set, 'day'}, "apple"),

  ?CHK({error, {{set, year}, {{2000, 2, 1}, 'end'}}},
       {2000, 2, 1}, {set, 'year'}, 'end'),
  ?CHK({2003, 2, 1}, {2000, 2, 1}, {set, 'year'}, 2003),

  % datetime mod plus
  Time = {12, 13, 14},

  ?CHK({{2000, 1, 2}, Time}, {{1999, 12, 31}, Time}, {'+', day}, 2),
  ?CHK({{2000, 2, 29}, Time}, {{1999, 12, 31}, Time}, {'+', month}, 2),
  ?CHK({{2001, 2, 28}, Time}, {{2000, 12, 31}, Time}, {'+', month}, 2),
  ?CHK({{2001, 12, 31}, Time}, {{1999, 12, 31}, Time}, {'+', year}, 2),
  ?CHK({error, {{'+', year}, {{1999, 12, 31}, "apple"}}},
       {1999, 12, 31}, {'+', year}, "apple"),

  % date mod minus
  ?CHK({{1999, 12, 31}, Time}, {{2000, 1, 2}, Time}, {'-', day}, 2),
  ?CHK({{1999, 11, 30}, Time}, {{2000, 1, 31}, Time}, {'-', month}, 2),
  ?CHK({{2000, 12, 28}, Time}, {{2001, 2, 28}, Time}, {'-', month}, 2),
  ?CHK({{1999, 12, 31}, Time}, {{2001, 12, 31}, Time}, {'-', year}, 2),
  ?CHK({error, {invalid, eval_operation, {'-', xx}, {{{2001, 12, 31}, Time}, 2}}},
       {{2001, 12, 31}, Time}, {'-', xx}, 2),
  ?CHK({error, {{'-', year}, {{{2001, 12, 31}, Time}, 2.2}}},
       {{2001, 12, 31}, Time}, {'-', year}, 2.2),
  ?CHK({error, {{'-', year}, {{{2001, 12, 31}, Time}, "apple"}}},
       {{2001, 12, 31}, Time}, {'-', year}, "apple"),

  % date set
  ?CHK({{2000, 2, 29}, Time}, {{2000, 2, 1}, Time}, {set, 'day'}, 'end'),
  ?CHK({{2000, 2, 11}, Time}, {{2000, 2, 1}, Time}, {set, 'day'}, 11),
  ?CHK({error, {{set, day}, {{{2000, 2, 1}, Time}, "apple"}}},
       {{2000, 2, 1}, Time}, {set, 'day'}, "apple"),

  ?CHK({{2000, 12, 29}, Time}, {{2000, 2, 29}, Time}, {set, 'month'}, 'end'),
  ?CHK({{2000, 11, 30}, Time}, {{2000, 1, 30}, Time}, {set, 'month'}, 11),
  ?CHK({error, {invalid_date, {{2000, 11, 31}, Time}}},
       {{2000, 1, 31}, Time}, {set, 'month'}, 11),
  ?CHK({error, {{set, day}, {{{2000, 2, 1}, Time}, "apple"}}},
       {{2000, 2, 1}, Time}, {set, 'day'}, "apple"),

  ?CHK({error, {{set, year}, {{{2000, 2, 1}, Time}, 'end'}}},
       {{2000, 2, 1}, Time}, {set, 'year'}, 'end'),
  ?CHK({{2003, 2, 1}, Time}, {{2000, 2, 1}, Time}, {set, 'year'}, 2003),

  ok.

test_eval_op_two_time() ->
  % time mod plus
  ?CHK({0, 0, 9}, {23, 59, 59}, {'+', 'second'}, 10),
  ?CHK({0, 9, 59}, {23, 59, 59}, {'+', 'minute'}, 10),
  ?CHK({9, 59, 59}, {23, 59, 59}, {'+', 'hour'}, 10),
  ?CHK({error, {{'+', second}, {{23, 59, 59}, 1.1}}},
       {23, 59, 59}, {'+', 'second'}, 1.1),
  ?CHK({error, {invalid, eval_operation, {'+', badop}, {{23, 59, 59}, 1}}},
       {23, 59, 59}, {'+', 'badop'}, 1),
  ?CHK({error, {{'+', hour}, {"apple", 10}}}, "apple", {'+', 'hour'}, 10),

  % time mod minus
  ?CHK({23, 59, 59}, {0, 0, 9}, {'-', 'second'}, 10),
  ?CHK({23, 59, 59}, {0, 9, 59}, {'-', 'minute'}, 10),
  ?CHK({23, 59, 59}, {9, 59, 59}, {'-', 'hour'}, 10),
  ?CHK({error, {{'-', second}, {{23, 59, 59}, 1.1}}},
       {23, 59, 59}, {'-', 'second'}, 1.1),
  ?CHK({error, {invalid, eval_operation, {'-', badop}, {{23, 59, 59}, 1}}},
       {23, 59, 59}, {'-', 'badop'}, 1),

  % time set
  ?CHK({10, 0, 25}, {10, 0, 0}, {'set', 'second'}, 25),
  ?CHK({10, 25, 0}, {10, 0, 0}, {'set', 'minute'}, 25),
  ?CHK({5, 0, 0}, {10, 0, 0}, {'set', 'hour'}, 5),
  ?CHK({error, {invalid_time, {10, 0, 60}}}, {10, 0, 0}, {'set', 'second'}, 60),
  ?CHK({error, {invalid_time, {10, 0, x}}}, {10, 0, 0}, {'set', 'second'}, x),
  ?CHK({error, {invalid_time, {10, 60, 0}}}, {10, 0, 0}, {'set', 'minute'}, 60),
  ?CHK({error, {invalid_time, {25, 0, 0}}}, {10, 0, 0}, {'set', 'hour'}, 25),
  ?CHK({error, {{set, hour}, {"apple", 5}}}, "apple", {'set', 'hour'}, 5),

  % datetime mod plus
  DateF = {2000, 12, 31},
  DateT = {2001, 1, 1},
  ?CHK({DateT, {0, 0, 9}}, {DateF, {23, 59, 59}}, {'+', 'second'}, 10),
  ?CHK({DateT, {0, 9, 59}}, {DateF, {23, 59, 59}}, {'+', 'minute'}, 10),
  ?CHK({DateT, {9, 59, 59}}, {DateF, {23, 59, 59}}, {'+', 'hour'}, 10),
  ?CHK({error, {{'+', second}, {{DateF, {23, 59, 59}}, 1.1}}},
       {DateF, {23, 59, 59}}, {'+', 'second'}, 1.1),
  ?CHK({error, {invalid, eval_operation,
                {'+', badop},
                {{DateF, {23, 59, 59}}, 1}}},
       {DateF, {23, 59, 59}}, {'+', 'badop'}, 1),

  % datetime mod minus
  DateF1 = {2001, 1, 1},
  DateT1 = {2000, 12, 31},
  ?CHK({DateT1, {23, 59, 59}}, {DateF1, {0, 0, 9}}, {'-', 'second'}, 10),
  ?CHK({DateT1, {23, 59, 59}}, {DateF1, {0, 9, 59}}, {'-', 'minute'}, 10),
  ?CHK({DateT1, {23, 59, 59}}, {DateF1, {9, 59, 59}}, {'-', 'hour'}, 10),
  ?CHK({error, {{'-', second}, {{DateF1, {23, 59, 59}}, 1.1}}},
       {DateF1, {23, 59, 59}}, {'-', 'second'}, 1.1),
  ?CHK({error, {invalid, eval_operation, {'-', badop},
                {{DateF1, {23, 59, 59}}, 1}}},
       {DateF1, {23, 59, 59}}, {'-', 'badop'}, 1),

  % datetime set
  Date = {2001, 1, 1},
  ?CHK({Date, {10, 0, 25}}, {Date, {10, 0, 0}}, {'set', 'second'}, 25),
  ?CHK({Date, {10, 25, 0}}, {Date, {10, 0, 0}}, {'set', 'minute'}, 25),
  ?CHK({Date, {5, 0, 0}}, {Date, {10, 0, 0}}, {'set', 'hour'}, 5),
  ?CHK({error, {invalid_time, {Date, {10, 0, 60}}}}, {Date, {10, 0, 0}},
       {'set', 'second'}, 60),
  ?CHK({error, {invalid_time, {Date, {10, 0, x}}}}, {Date, {10, 0, 0}},
       {'set', 'second'}, x),
  ?CHK({error, {invalid_time, {Date, {10, 60, 0}}}}, {Date, {10, 0, 0}},
       {'set', 'minute'}, 60),
  ?CHK({error, {invalid_time, {Date, {25, 0, 0}}}}, {Date, {10, 0, 0}},
       {'set', 'hour'}, 25).

test_eval_op_two_list() ->
  ?CHK([1, 2, 3], [2, 3], {'++', head}, 1),
  ?CHK([1, 2, 3], [1, 2], {'++', tail}, 3),
  ?CHK({error, {invalid, eval_operation, {'++', other}, {[1, 2], 3}}}, [1, 2],
       {'++', other}, 3),
  ?CHK({error, {{'++', tail}, {x, 3}}}, x, {'++', tail}, 3),
  ok.