%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2013, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 15 Nov 2013 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(mpower_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mpower.hrl").


%%%-------------------------------------------------------------------
%%% Setup / Cleanup
%%%-------------------------------------------------------------------
setup() ->
    application:set_env(mpower, debug, true),
    application:set_env(mpower, api_keys, ?MPOWER_TEST_API_KEYS),
    application:start(mpower),
    ok.

teardown(_) ->
    application:set_env(mpower, debug, undefined),
    application:set_env(mpower, api_keys, undefined),
    application:stop(mpower).


%%%-------------------------------------------------------------------
%%% unit tests
%%%-------------------------------------------------------------------
%% main test fixture
mpower_test_() ->
    {spawn, 
     {setup,
      fun setup/0,
      fun teardown/1,
      [
       fun invoice_create/0,
       fun invoice_confirm/0,
       fun opr_create/0,
       fun opr_charge/0,
       fun direct_pay/0,
       fun direct_card/0
      ]
     }
    }.

invoice_create() ->
     ?assertEqual(1, 1).
invoice_confirm() ->
     ?assertEqual(1, 1).
opr_create() ->
     ?assertEqual(1, 1).
opr_charge() ->
     ?assertEqual(1, 1).
direct_pay() ->
     ?assertEqual(1, 1).
direct_card() ->             
    ?assertEqual(1, 1).
