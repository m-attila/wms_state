%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2019 08:44
%%%-------------------------------------------------------------------
-author("Attila Makra").

-ifndef(__WMS_STATE_HRL).
-define(__WMS_STATE_HRL, wms_state_hrl).

-type identifier_name() :: binary().

%% =============================================================================
%% Types
%% =============================================================================

-type variable_type() :: private | global.
-type variable_reference() :: {variable_type(), identifier_name()}.
-type literal(T) :: T.
-type literal() :: literal(term()).

-type variable_or_literal() :: literal() | variable_reference().
-type source() :: variable_or_literal().
-type destination() :: variable_reference().

-type transaction_fun() :: fun((Environment :: map())->
  {ok, NewEnvironment :: map()} | {error, term()}).

-endif.
