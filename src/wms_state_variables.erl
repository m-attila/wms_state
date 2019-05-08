%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% State-variable handling for WMS system.
%%% @end
%%% Created : 08. May 2019 08:45
%%%-------------------------------------------------------------------
-module(wms_state_variables).
-author("Attila Makra").

-export([move_var/4,
         eval_var/3,
         eval_operation/2,
         build_transaction/7]).

-include("wms_state.hrl").
-include("wms_state_variable_callbacks.hrl").

%% API

%% -----------------------------------------------------------------------------
%% Eval variable reference
%% -----------------------------------------------------------------------------

-spec eval_var(variable_or_literal() | {literal, literal() | undefined},
               Impl :: atom(), Environment :: map()) ->
                {ok, literal()} | {error, term()}.
eval_var({private, _} = Reference, Impl, Environment) ->
  apply(Impl, get_variable, [Environment, Reference]);
eval_var({global, _} = Reference, Impl, Environment) ->
  apply(Impl, get_variable, [Environment, Reference]);
eval_var({literal, Literal}, _, _) ->
  {ok, Literal};
eval_var(Literal, _, _) ->
  {ok, Literal}.

%% -----------------------------------------------------------------------------
%% Eval operation
%% -----------------------------------------------------------------------------

-spec eval_operation(literal(), {atom(), literal() | undefined}) ->
  {ok, literal() | {literal(), [literal()]}} |
  {error, term()}.
eval_operation(Literal, {Operation, undefined}) ->
  % operation without argument.
  Op = case Operation of
         {Complex} ->
           % Operation and its arg embedded in tuple. If Operation is a complex
           % expression i.e {'+', day} must use by {{'+', day'}} to differentiate
           % operator and arg composition i.e {'+', 12}
           Complex;
         _ ->
           Operation
       end,
  wms_state:eval_operation(Literal, Op);
eval_operation(Literal, {Operation, OpArgLiteral}) ->
  % operation with argument
  wms_state:eval_operation(Literal, Operation, OpArgLiteral).

-spec move_var(source() | {source(), term()}, destination(), atom(), map()) ->
  {ok, map()} | {error, term()}.
move_var({{SType, SID}, Op}, Destination, Impl, Environment) ->
  % move with source operator
  move_var(SType, SID, Op, Destination, Impl, Environment);
move_var({SType, SID}, Destination, Impl, Environment)
  when SType =:= private orelse SType =:= global ->
  % move without source Operator
  move_var(SType, SID, 'nop', Destination, Impl, Environment);
move_var({SLiteral, Op}, Destination, Impl, Environment) ->
  % move source literal with operator
  move_var(literal, SLiteral, Op, Destination, Impl, Environment);
move_var(SLiteral, Destination, Impl, Environment) ->
  % move source literal
  move_var(literal, SLiteral, 'nop', Destination, Impl, Environment).

-spec move_var(SourceType :: private | global | literal,
               SLiteralOrID :: identifier_name() | term(),
               Op :: atom() | {Op :: atom(), OpArg :: variable_or_literal()},
               Destination :: variable_reference(),
               Impl :: atom(),
               Environment :: map()) ->
                {ok, map()} | {error, term()}.
move_var(SourceType, SLiteralOrID, {Op, {Type, OpArgID}}, Destination, Impl,
         Environment) ->
  % operator with variable reference argument
  move_var(SourceType, SLiteralOrID, Op, Type, OpArgID, Destination, Impl,
           Environment);
move_var(SourceType, SLiteralOrID, {Op, OpArgLit}, Destination, Impl,
         Environment) ->
  % operator with literal argument
  move_var(SourceType, SLiteralOrID, Op, literal, OpArgLit, Destination, Impl,
           Environment);
move_var(SourceType, SLiteralOrID, Op, Destination, Impl, Environment) ->
  % operator without argument
  move_var(SourceType, SLiteralOrID, Op, literal, undefined, Destination, Impl,
           Environment).

-spec move_var(SourceType :: private | global | literal,
               SLiteralOrID :: identifier_name() | term(),
               Op :: atom(),
               OpType :: private | global | literal,
               OpLiteralOrID :: identifier_name() | term() | undefined,
               Destination :: variable_reference(),
               Impl :: atom(),
               Environment :: map()) ->
                {ok, map()} | {error, term()}.
move_var(SourceType, SLiteralOrID, Op, OpType, OpLiteralOrID, Destination, Impl,
         StartEnvironment) ->
  Transaction =
    ?MODULE:build_transaction(SourceType,
                              SLiteralOrID,
                              Op,
                              OpType,
                              OpLiteralOrID,
                              Destination,
                              Impl),
  run_transaction(Transaction, Impl, StartEnvironment).

-spec build_transaction(SourceType :: private | global | literal,
                        SLiteralOrID :: identifier_name() | term(),
                        Op :: atom(),
                        OpType :: private | global | literal,
                        OpLiteralOrID :: identifier_name() | term() | undefined,
                        Destination :: variable_reference(),
                        Impl :: atom()) ->
                         fun((map()) ->
                           {ok, map()}).
build_transaction(SourceType, SLiteralOrID, Op, OpType, OpLiteralOrID, Destination,
                  Impl) ->
  fun(Environment) ->
    {ok, SourceValue} = eval_var({SourceType, SLiteralOrID}, Impl, Environment),
    {ok, OpArgValue} = eval_var({OpType, OpLiteralOrID}, Impl, Environment),
    {ok, OperatorResult} = eval_operation(SourceValue, {Op, OpArgValue}),

    SetCommands =
      case OperatorResult of
        {Element, RestElements} when is_list(RestElements) ->
          % list manipulation with two results
          [{{SourceType, SLiteralOrID}, RestElements},
           {Destination, Element}];
        _ ->
          % only one result
          [{Destination, OperatorResult}]
      end,

    NewEnvironment =
      lists:foldl(
        fun({{literal, _}, _}, E) ->
          E;
           ({Dest, Value}, E) ->
             {ok, E1} = apply(Impl, set_variable, [E, Dest, Value, true]),
             E1
        end,
        Environment, SetCommands),
    {ok, NewEnvironment}
  end.

-spec run_transaction(transaction_fun(), atom(), map()) ->
  {ok, map()} | {error, term()}.
run_transaction(Transaction, Impl, StartEnvironment) ->
  apply(Impl, transaction, [StartEnvironment, fun(Environment) ->
    try
      Transaction(Environment)
    catch error : {badmatch, R} ->
      R;
      _ : R1 ->
        {error, R1}
    end end]).