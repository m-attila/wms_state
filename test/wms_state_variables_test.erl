%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Unit test for state_variables.
%%% @end
%%% Created : 08. May 2019 11:19
%%%-------------------------------------------------------------------
-module(wms_state_variables_test).
-author("Attila Makra").

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Definitions
%% =============================================================================

-define(CHK_VAR(Expected, Variable, Environment),
  ?assertEqual(Expected,
               wms_state:eval_var(
                 Variable,
                 wms_state_variables_adapter, Environment))).

-define(MOVE(Expected, Src, Dest, Env),
  begin
    put(result, wms_state:move_var(Src, Dest,
                                   wms_state_variables_adapter, Env)),
    ?assertMatch(Expected, get(result)),

    unpack(get(result))
  end
).

%% =============================================================================
%% Test functions
%% =============================================================================

eval_var_test() ->
  Var1 = {private, <<"var1">>},
  Val1 = 12345,

  Var2 = {global, <<"var2">>},
  Val2 = 54321,

  Var3 = {global, <<"var3">>},

  Literal = 666666,

  Environment = #{Var1 => Val1, Var2 => Val2},

  ?CHK_VAR({ok, Val1}, Var1, Environment),
  ?CHK_VAR({ok, Val2}, Var2, Environment),
  ?CHK_VAR({ok, Literal}, {literal, Literal}, Environment),
  ?CHK_VAR({ok, Literal}, Literal, Environment),
  ?CHK_VAR({error, {not_found, Var3}}, Var3, Environment).

eval_operation_test() ->
  Literal1 = 10,
  Literal2 = 20,
  Literal3 = true,
  Literal4 = [1, 2, 3, 4],

  % operator with argument
  ?assertEqual({ok, 10 + 20},
               wms_state_variables:eval_operation(Literal1,
                                                  {'+', Literal2})),
  % operator without argument
  ?assertEqual({ok, false},
               wms_state_variables:eval_operation(Literal3,
                                                  {'!', undefined})),
  ?assertEqual({ok, {1, [2, 3, 4]}},
               wms_state_variables:eval_operation(Literal4,
                                                  {{{'--', head}}, undefined})),
  ok.

move_test() ->
  VarP1 = {private, <<"VarP1">>},
  VarP2 = {private, <<"VarP2">>},
  VarP3 = {private, <<"VarP3">>},
  VarG1 = {global, <<"VarG1">>},
  VarG2 = {global, <<"VarG2">>},

  % move literal to private variable
  Env = ?MOVE({ok, _}, 123, VarP1, #{}),
  ?CHK_VAR({ok, 123}, VarP1, Env),

  % move private variable to another private
  Env1 = ?MOVE({ok, _}, VarP1, VarP2, Env),
  ?CHK_VAR({ok, 123}, VarP2, Env1),

  % move literal to global variable
  Env2 = ?MOVE({ok, _}, 123, VarG1, #{}),
  ?CHK_VAR({ok, 123}, VarG1, Env2),

  % move global variable to another global
  Env3 = ?MOVE({ok, _}, VarG1, VarG2, Env2),
  ?CHK_VAR({ok, 123}, VarG2, Env3),

  % move literal with opless argument
  Env4 = ?MOVE({ok, _}, {false, '!'}, VarP1, #{}),
  ?CHK_VAR({ok, true}, VarP1, Env4),

  % move source with opless argument
  Env5 = ?MOVE({ok, _}, {VarP1, '!'}, VarP2, #{VarP1 => false}),
  ?CHK_VAR({ok, true}, VarP2, Env5),

  % move source with opless argument list operation
  Env6 = ?MOVE({ok, _}, {VarP1, {{'--', head}}}, VarP2, #{VarP1 => [1, 2, 3]}),
  ?CHK_VAR({ok, 1}, VarP2, Env6),
  ?CHK_VAR({ok, [2, 3]}, VarP1, Env6),

  % move source with opless argument list operation
  Env61 = ?MOVE({ok, _}, {[1, 2, 3], {{'--', head}}}, VarP2, #{}),
  ?CHK_VAR({ok, 1}, VarP2, Env61),

  % move source literal with ops argument literal
  Env7 = ?MOVE({ok, _}, {10, {'+', 20}}, VarP1, #{}),
  ?CHK_VAR({ok, 10 + 20}, VarP1, Env7),

  % move source variable with ops argument literal
  Env8 = ?MOVE({ok, _}, {VarP1, {'+', 20}}, VarP2, #{VarP1 => 10}),
  ?CHK_VAR({ok, 10 + 20}, VarP2, Env8),

  % move source literal with ops argument variable
  Env9 = ?MOVE({ok, _}, {10, {'+', VarP1}}, VarP2, #{VarP1 => 20}),
  ?CHK_VAR({ok, 10 + 20}, VarP2, Env9),

  % move source variable with ops argument variable
  EnvA = ?MOVE({ok, _}, {VarP1, {'+', VarP2}}, VarP3, #{VarP1 => 10, VarP2 =>20}),
  ?CHK_VAR({ok, 10 + 20}, VarP3, EnvA),

  ok.

transaction_error_test() ->
  VarP1 = {private, <<"VarP1">>},

  % move literal to private variable
  ?MOVE({error, {variable, {not_found, {private, <<"no_var">>}}, #{}}},
        {private, <<"no_var">>}, VarP1, #{}),

  try
    ok = meck:new(wms_state_variables, [passthrough]),
    meck:expect(wms_state_variables, build_transaction,
                fun(_SourceType, _SLiteralOrID, _Op, _OpType,
                    _OpLiteralOrID, _Destination, _Impl) ->
                  fun(_Environment) ->
                    throw(any_error)
                  end
                end),
    ?MOVE({error, any_error}, 123, VarP1, #{})

  after
    meck:unload(wms_state_variables)
  end.

%% =============================================================================
%% Private functions
%% =============================================================================

unpack({_, Res}) ->
  Res.