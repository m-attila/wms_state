%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Callback definitions for wms_state_variables module.
%%% @end
%%% Created : 08. May 2019 18:53
%%%-------------------------------------------------------------------
-author("Attila Makra").

%% =============================================================================
%% Callbacks
%% =============================================================================
-callback get_variable(Environment :: map(), Reference :: variable_reference()) ->
  {ok, Value :: literal()} | {error, Reason :: term()}.

-callback set_variable(Environment :: map(),
                       Reference :: variable_reference(),
                       Value :: literal(),
                       InTransaction :: boolean()) ->
                        {ok, NewEnvironment :: map()} | {error, Reason :: term()}.

-callback transaction(StartEnvironment :: map(),
                      Transaction :: transaction_fun()) ->
                       {ok, map()} | {error, term()}.