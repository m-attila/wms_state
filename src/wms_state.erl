-module(wms_state).

%% API exports
-export([eval_operation/3,
         eval_operation/2]).

%%====================================================================
%% API functions
%%====================================================================



%% -----------------------------------------------------------------------------
%% Evaluate operations functions
%% -----------------------------------------------------------------------------
-spec eval_operation(term(), term(), term()) ->
  term() | {error, term()}.
eval_operation(Literal1, Operation, Literal2) ->
  wms_state_operator:eval_operation(Literal1, Operation, Literal2).

-spec eval_operation(term(), term()) ->
  term() | {error, term()}.
eval_operation(Literal, Operation) ->
  wms_common_operator:eval_operation(Literal, Operation).

