-module(wms_state).

-include("wms_state.hrl").

%% API exports
-export([eval_operation/3,
         eval_operation/2,
         move_var/4,
         eval_var/3]).

%%====================================================================
%% API functions
%%====================================================================

%% -----------------------------------------------------------------------------
%% Evaluate operations functions
%% -----------------------------------------------------------------------------

% két operandusú operátor kiértékelése literálon
-spec eval_operation(term(), term(), term()) ->
  {ok, term()} | {error, term()}.
eval_operation(Literal1, Operation, Literal2) ->
  ok(wms_state_operator:eval_operation(Literal1, Operation, Literal2)).

% egy operandusú operátor kiértékelése literálon
-spec eval_operation(term(), term()) ->
  {ok, term()} | {error, term()}.
eval_operation(Literal, Operation) ->
  ok(wms_state_operator:eval_operation(Literal, Operation)).

%% =============================================================================
%% Variable handling
%% =============================================================================

-spec eval_var(variable_or_literal() | {literal, literal() | undefined},
               WMSStateVariablesBehaviour :: atom(), Environment :: map()) ->
                {ok, literal()} | {error, term()}.
eval_var(ReferenceOrLiteral, WMSStateVariablesBehaviour, Environment) ->
  wms_state_variables:eval_var(ReferenceOrLiteral,
                               WMSStateVariablesBehaviour, Environment).

-spec move_var(source() | {source(), term()}, destination(), atom(), map()) ->
  {ok, map()} | {error, term()}.
move_var(Source, Destination, WMSStateVariablesBehaviour, Environment) ->
  wms_state_variables:move_var(Source, Destination,
                               WMSStateVariablesBehaviour, Environment).

%% =============================================================================
%% Private function
%% =============================================================================

ok({error, _} = Error) ->
  Error;
ok(Result) ->
  {ok, Result}.