%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2019 08:03
%%%-------------------------------------------------------------------
-module(wms_state_operator).
-author("Attila Makra").


%% API
-export([eval_operation/3,
         eval_operation/2]).

%% =============================================================================
%% Declarations
%% =============================================================================

-define(ONE_ARG_OPS, #{
  'nop' => fun nop_operator/2,
  '!' => fun exclamation_operator/2,
  {'--', head} => fun list_operator/2,
  {'--', tail} => fun list_operator/2,
  {'?', head} => fun list_operator/2,
  {'?', tail} => fun list_operator/2
}).

-define(TWO_ARG_OPS, #{
  '+' => fun plus_operator/3,
  '-' => fun minus_operator/3,
  '*' => fun multiple_operator/3,
  '/' => fun divisor_operator/3,

  {'+', day} => fun date_mod_operator/3,
  {'+', month} => fun date_mod_operator/3,
  {'+', year} => fun date_mod_operator/3,
  {'-', day} => fun date_mod_operator/3,
  {'-', month} => fun date_mod_operator/3,
  {'-', year} => fun date_mod_operator/3,

  {set, day} => fun date_set_operator/3,
  {set, month} => fun date_set_operator/3,
  {set, year} => fun date_set_operator/3,

  {'+', second} => fun time_mod_operator/3,
  {'+', minute} => fun time_mod_operator/3,
  {'+', hour} => fun time_mod_operator/3,
  {'-', second} => fun time_mod_operator/3,
  {'-', minute} => fun time_mod_operator/3,
  {'-', hour} => fun time_mod_operator/3,

  {set, second} => fun time_set_operator/3,
  {set, minute} => fun time_set_operator/3,
  {set, hour} => fun time_set_operator/3,

  {'++', head} => fun list_operator/3,
  {'++', tail} => fun list_operator/3


}).

%% -----------------------------------------------------------------------------
%% Types
%% -----------------------------------------------------------------------------
-type date() :: {integer(), byte(), byte()}.
-type time() :: {byte(), byte(), byte()}.
-type datetime() :: {date(), time()}.


%% =============================================================================
%% API functions
%% =============================================================================

% két operandusú operátor kiértékelése literálon
-spec eval_operation(term(), term(), term()) ->
  term() | {error, term()}.
eval_operation(Literal1, Operation, Literal2) ->
  case maps:get(Operation, ?TWO_ARG_OPS, undefined) of
    undefined ->
      {error, {invalid, eval_operation, Operation, {Literal1, Literal2}}};
    Fun ->
      Fun(Literal1, Operation, Literal2)
  end.

% egy operandusú operátor kiértékelése literálon
-spec eval_operation(term(), term()) ->
  term() | {error, term()}.
eval_operation(Literal, Operation) ->
  case maps:get(Operation, ?ONE_ARG_OPS, undefined) of
    undefined ->
      {error, {invalid, eval_operation, Operation, Literal}};
    Fun ->
      Fun(Literal, Operation)
  end.

%% =============================================================================
%% Operators with arguments
%% =============================================================================

%% -----------------------------------------------------------------------------
%% Base operations
%% -----------------------------------------------------------------------------

-spec plus_operator([term()] | number() | boolean() | binary(),
                    term(),
                    [term()] | number() | boolean() | binary()) ->
                     [term()] | number()| boolean() | binary() | {error, term()}.
plus_operator(Literal1, _, Literal2) when is_list(Literal1)
                                          andalso is_list(Literal2) ->
  Literal1 ++ Literal2;
plus_operator(Literal1, _, Literal2) when is_number(Literal1)
                                          andalso is_number(Literal2) ->
  Literal1 + Literal2;
plus_operator(Literal1, _, Literal2) when is_boolean(Literal1)
                                          andalso is_boolean(Literal2) ->
  Literal1 or Literal2;
plus_operator(<<Literal1/binary>>, _, <<Literal2/binary>>) ->
  <<Literal1/binary, Literal2/binary>>;
plus_operator(Literal1, Op, Literal2) ->
  {error, {Op, {Literal1, Literal2}}}.

-spec minus_operator(number() | boolean() | [term()],
                     term(),
                     number() | boolean() | [term()]) ->
                      {number()| boolean() | [term()]} | {error, term()}.
minus_operator(Literal1, _, Literal2) when is_number(Literal1)
                                           andalso is_number(Literal2) ->
  Literal1 - Literal2;
minus_operator(Literal1, _, Literal2) when is_boolean(Literal1)
                                           andalso is_boolean(Literal2) ->
  Literal1 xor Literal2;
minus_operator(Literal1, _, Literal2) when is_list(Literal1)
                                           andalso is_list(Literal2) ->
  lists:subtract(Literal1, Literal2);
minus_operator(Literal1, Op, Literal2) ->
  {error, {Op, {Literal1, Literal2}}}.

-spec multiple_operator(number() | boolean() | [term()],
                        term(),
                        number() | boolean() | [term()]) ->
                         {number()| boolean()| [term()]} | {error, term()}.
multiple_operator(Literal1, _, Literal2) when is_number(Literal1)
                                              andalso is_number(Literal2) ->
  Literal1 * Literal2;
multiple_operator(Literal1, _, Literal2) when is_boolean(Literal1)
                                              andalso is_boolean(Literal2) ->
  Literal1 and Literal2;
multiple_operator(Literal1, _, Literal2) when is_list(Literal1)
                                              andalso is_list(Literal2) ->
  lists:usort(
    lists:filter(
      fun(E) ->
        lists:member(E, Literal2) end, Literal1) ++
      lists:filter(
        fun(E) ->
          lists:member(E, Literal1) end, Literal2));
multiple_operator(Literal1, Op, Literal2) ->
  {error, {Op, {Literal1, Literal2}}}.

-spec divisor_operator(integer() | float(),
                       term(),
                       integer() | float()) ->
                        {integer() | float()} | {error, term()}.
divisor_operator(Literal1, _, Literal2) when is_integer(Literal1)
                                             andalso is_integer(Literal2) ->
  Literal1 div Literal2;
divisor_operator(Literal1, _, Literal2) when is_number(Literal1)
                                             andalso is_number(Literal2) ->
  Literal1 / Literal2;
divisor_operator(Literal1, Op, Literal2) ->
  {error, {Op, {Literal1, Literal2}}}.

%% -----------------------------------------------------------------------------
%% Date operations
%% -----------------------------------------------------------------------------
-spec date_mod_operator(date() | datetime(),
                        term(),
                        integer()) ->
                         date() | datetime() | {error, term()}.
date_mod_operator(Literal1, Op, Literal2) ->
  case is(Literal1, [date, datetime]) of
    true ->
      do_date_mod_operator(Literal1, Op, Literal2);
    false ->
      {error, {Op, {Literal1, Literal2}}}
  end.

-spec do_date_mod_operator(date() | datetime(),
                           term(),
                           integer()) ->
                            date() | datetime() | {error, term()}.
do_date_mod_operator(Literal1, {'+', day}, Literal2) when is_integer(Literal2) ->
  add(Literal1, Literal2, day);
do_date_mod_operator(Literal1, {'+', month}, Literal2) when is_integer(Literal2) ->
  add(Literal1, Literal2, month);
do_date_mod_operator(Literal1, {'+', year}, Literal2) when is_integer(Literal2) ->
  add(Literal1, Literal2, year);
do_date_mod_operator(Literal1, {'-', day}, Literal2) when is_integer(Literal2) ->
  add(Literal1, -Literal2, day);
do_date_mod_operator(Literal1, {'-', month}, Literal2) when is_integer(Literal2) ->
  add(Literal1, -Literal2, month);
do_date_mod_operator(Literal1, {'-', year}, Literal2) when is_integer(Literal2) ->
  add(Literal1, -Literal2, year);
do_date_mod_operator(Literal1, Op, Literal2) ->
  {error, {Op, {Literal1, Literal2}}}.

-spec date_set_operator(date() | datetime(),
                        term(),
                        integer() | 'end') ->
                         date() | datetime() | {error, term()}.
date_set_operator(Literal1, {_, What} = Op, Literal2)
  when is_integer(Literal2)
       orelse (Literal2 =:= 'end' andalso What =/= 'year') ->

  case is(Literal1, [date, datetime]) of
    true ->
      set(Literal1, Literal2, What);
    false ->
      {error, {Op, {Literal1, Literal2}}}
  end;
date_set_operator(Literal1, Op, Literal2) ->
  {error, {Op, {Literal1, Literal2}}}.

%% -----------------------------------------------------------------------------
%% Time operations
%% -----------------------------------------------------------------------------

-spec time_mod_operator(time() | datetime(), term(), integer()) ->
  time() | datetime() | {error, term()}.
time_mod_operator(Literal1, Op, Literal2) ->
  case is(Literal1, [time, datetime]) of
    true ->
      do_time_mod_operator(Literal1, Op, Literal2);
    false ->
      {error, {Op, {Literal1, Literal2}}}
  end.

-spec do_time_mod_operator(time() | datetime(), term(), integer()) ->
  time() | datetime() | {error, term()}.
do_time_mod_operator(Literal1, {'+', second}, Literal2) when is_integer(Literal2) ->
  add_t(Literal1, Literal2, second);
do_time_mod_operator(Literal1, {'+', minute}, Literal2) when is_integer(Literal2) ->
  add_t(Literal1, Literal2, minute);
do_time_mod_operator(Literal1, {'+', hour}, Literal2) when is_integer(Literal2) ->
  add_t(Literal1, Literal2, hour);
do_time_mod_operator(Literal1, {'-', second}, Literal2) when is_integer(Literal2) ->
  add_t(Literal1, - Literal2, second);
do_time_mod_operator(Literal1, {'-', minute}, Literal2) when is_integer(Literal2) ->
  add_t(Literal1, - Literal2, minute);
do_time_mod_operator(Literal1, {'-', hour}, Literal2) when is_integer(Literal2) ->
  add_t(Literal1, - Literal2, hour);
do_time_mod_operator(Literal1, Op, Literal2) ->
  {error, {Op, {Literal1, Literal2}}}.

-spec time_set_operator(time() | datetime(), term(), integer()) ->
  time() | datetime() | {error, term()}.
time_set_operator(Literal1, {_, What} = Op, Literal2) ->
  case is(Literal1, [time, datetime]) of
    true ->
      set_t(Literal1, Literal2, What);
    false ->
      {error, {Op, {Literal1, Literal2}}}
  end.

%% -----------------------------------------------------------------------------
%% List operations
%% -----------------------------------------------------------------------------

-spec list_operator([term()], term(), term()) ->
  [term()] | {error, term()}.
list_operator(List, {'++', head}, Element) when is_list(List) ->
  [Element | List];
list_operator(List, {'++', tail}, Element) when is_list(List) ->
  List ++ [Element];
list_operator(Literal1, Op, Literal2) ->
  {error, {Op, {Literal1, Literal2}}}.

%% =============================================================================
%% Operators without arguments
%% =============================================================================

-spec nop_operator(term(), term()) ->
  term().
nop_operator(Literal, _) ->
  Literal.

-spec exclamation_operator(boolean(), term()) ->
  boolean() | {error, term()}.
exclamation_operator(Literal, _) when is_boolean(Literal) ->
  not Literal;
exclamation_operator(Literal, Op) ->
  {error, {Op, Literal}}.

-spec list_operator([term()], term()) ->
  {term(), [term()]} | {error, term()}.
list_operator([], {_, head} = Op) ->
  {error, {Op, {[]}}};
list_operator([H | T], {'--', head}) ->
  {H, T};
list_operator([H | _] = List, {'?', head}) ->
  {H, List};
list_operator([], {_, tail} = Op) ->
  {error, {Op, {[]}}};
list_operator([H], {'--', tail}) ->
  {H, []};
list_operator([H], {'?', tail}) ->
  {H, [H]};
list_operator(List, {'--', tail}) when is_list(List) ->
  {lists:last(List), lists:droplast(List)};
list_operator(List, {'?', tail}) when is_list(List) ->
  {lists:last(List), List};
list_operator(Literal, Op) ->
  {error, {Op, Literal}}.


%% =============================================================================
%% Private functions
%% =============================================================================

-spec add(date() | datetime(), integer(), atom()) ->
  date() | datetime().
add({Date, Time}, Number, What) when is_integer(Number) ->
  {add(Date, Number, What), Time};
add(Date, Number, day) when is_integer(Number) ->
  edate:shift(Date, Number, days);
add(Date, Number, month) when is_integer(Number) ->
  edate:shift(Date, Number, months);
add(Date, Number, year) when is_integer(Number) ->
  edate:shift(Date, Number, years).

-spec add_t(time() | datetime(), integer(), atom()) ->
  time() | datetime().
add_t({_, _, _} = Time, Number, second) ->
  S = time_to_second(Time),
  {_, Ret} = second_to_time(S + Number),
  Ret;
add_t({_, _, _} = Time, Number, minute) ->
  S = time_to_second(Time),
  {_, Ret} = second_to_time(S + Number * 60),
  Ret;
add_t({_, _, _} = Time, Number, hour) ->
  S = time_to_second(Time),
  {_, Ret} = second_to_time(S + Number * 3600),
  Ret;
add_t({Date, Time}, Number, second) ->
  S = time_to_second(Time),
  {DayOffset, NewTime} = second_to_time(S + Number),
  add({Date, NewTime}, DayOffset, day);
add_t({Date, Time}, Number, minute) ->
  S = time_to_second(Time),
  {DayOffset, NewTime} = second_to_time(S + Number * 60),
  add({Date, NewTime}, DayOffset, day);
add_t({Date, Time}, Number, hour) ->
  S = time_to_second(Time),
  {DayOffset, NewTime} = second_to_time(S + Number * 3600),
  add({Date, NewTime}, DayOffset, day).

-spec set(date() | datetime(), integer() | 'end', atom()) ->
  date() | datetime() | {error, term()}.
set({_, _, _} = Date, 'end', day) ->
  edate:end_of_month(Date);
set({Y, M, _}, Number, day) ->
  check_valid_date({Y, M, Number});
set({Y, _, D}, 'end', month) ->
  check_valid_date({Y, 12, D});
set({Y, _, D}, Number, month) ->
  check_valid_date({Y, Number, D});
set({_, M, D}, Number, year) ->
  check_valid_date({Number, M, D});
set({Date, Time}, Value, What) ->
  case set(Date, Value, What) of
    {error, {invalid_date, {Y, M, D}}} ->
      {error, {invalid_date, {{Y, M, D}, Time}}};
    Other ->
      {Other, Time}
  end.

-spec set_t(time() | datetime(), integer(), atom()) ->
  time() | datetime() | {error, term()}.
set_t({H, M, _}, Number, second) ->
  check_valid_time({H, M, Number});
set_t({H, _, S}, Number, minute) ->
  check_valid_time({H, Number, S});
set_t({_, M, S}, Number, hour) ->
  check_valid_time({Number, M, S});
set_t({Date, Time}, Number, What) ->
  case set_t(Time, Number, What) of
    {error, {invalid_time, {H, M, S}}} ->
      {error, {invalid_time, {Date, {H, M, S}}}};
    Other ->
      {Date, Other}
  end.

-spec check_valid_date(date() | datetime()) ->
  date() | {error, term()}.
check_valid_date({_, _, _} = Date) ->
  case calendar:valid_date(Date) of
    true ->
      Date;
    false ->
      {error, {invalid_date, Date}}
  end.

-spec time_to_second(time()) ->
  integer().
time_to_second({H, M, S}) ->
  H * 3600 + M * 60 + S.

-spec second_to_time(integer()) ->
  {Days :: integer(), time()}.
second_to_time(Seconds) when Seconds >= 0 ->
  {Seconds div 86400,
   calendar:seconds_to_time(Seconds rem 86400)};
second_to_time(Seconds) ->
  {-1 + (Seconds div 86400),
   calendar:seconds_to_time(86400 + (Seconds rem 86400))}.

-spec check_valid_time(time()) ->
  time() | {error, term()}.
check_valid_time({H, M, S} = Time) ->
  case H >= 0 andalso H =< 23
       andalso M >= 0 andalso M =< 59
       andalso S >= 0 andalso S =< 59 of
    true ->
      Time;
    false ->
      {error, {invalid_time, Time}}
  end.

-spec is(term(), [atom()]) ->
  boolean().
is(_, []) ->
  false;
is(Literal, [Type | RestTypes]) ->
  case {Type, Literal} of
    {date, {_, _, _}} ->
      true;
    {time, {_, _, _}} ->
      true;
    {datetime, {{_, _, _}, {_, _, _}}} ->
      true;
    _ ->
      is(Literal, RestTypes)
  end.