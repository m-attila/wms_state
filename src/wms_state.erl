-module(wms_state).

-include("wms_state.hrl").

%% API exports
-export([eval_operation/3,
         eval_operation/2,
         move_var/4]).

%%====================================================================
%% API functions
%%====================================================================

%% -----------------------------------------------------------------------------
%% Evaluate operations functions
%% -----------------------------------------------------------------------------
-spec eval_operation(term(), term(), term()) ->
  {ok, term()} | {error, term()}.
eval_operation(Literal1, Operation, Literal2) ->
  ok(wms_state_operator:eval_operation(Literal1, Operation, Literal2)).

-spec eval_operation(term(), term()) ->
  {ok, term()} | {error, term()}.
eval_operation(Literal, Operation) ->
  ok(wms_state_operator:eval_operation(Literal, Operation)).

%% =============================================================================
%% Move variable
%% =============================================================================
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