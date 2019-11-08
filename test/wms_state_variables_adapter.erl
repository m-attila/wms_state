%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Test implementation for wms_state_variables behaviour.
%%% @end
%%% Created : 08. May 2019 11:24
%%%-------------------------------------------------------------------
-module(wms_state_variables_adapter).
-author("Attila Makra").
-behaviour(wms_state_variables).

-include("wms_state.hrl").

-export([get_variable/2,
         transaction/2,
         set_variable/4,
         save_state/1,
         drop_state/1,
         load_state/1]).

%% =============================================================================
%% Callback implementation
%% =============================================================================
-spec get_variable(Environment :: map(), Reference :: variable_reference()) ->
  {ok, Value :: literal()} | {error, Reason :: term()}.


get_variable(Environment, Reference) ->
  case maps:get(Reference, Environment, undefined) of
    undefined ->
      {error, {not_found, Reference}};
    Value ->
      {ok, Value}
  end.

-spec set_variable(Environment :: map(),
                   Reference :: variable_reference(),
                   Value :: literal(),
                   InTransaction :: boolean()) ->
                    {ok, NewEnvironment :: map()} | {error, Reason :: term()}.
set_variable(Environment, Reference, Value, _InTransaction) ->
  {ok, Environment#{Reference => Value}}.

-spec transaction(StartEnvironment :: map(),
                  Transaction :: fun((Environment :: map()) ->
                    {ok, NewEnvironment :: map()} | {error, term()})) ->
                   {ok, map()} | {error, term()}.

transaction(StartEnvironment, Transaction) ->
  Transaction(StartEnvironment).

-spec save_state(State :: map()) ->
  ok.
save_state(_State) ->
  ok.

-spec drop_state(State :: map()) ->
  ok.
drop_state(_State) ->
  ok.

-spec load_state(InitialState :: map()) ->
  not_found | map().
load_state(_) ->
  throw(not_implemented).


