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


%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
mpower_test_() ->
    {spawn, 
     {setup,
      fun setup/0,
      fun teardown/1,
      [
       {"Create invoice",
        fun create_invoice/0},
       {"Confirm/check invoice status",
        fun confirm_invoice/0},
       {"Start an OPR(Onsite Payment Request)",
        fun create_opr/0},
       {"Complete an OPR process",
        fun charge_opr/0},
       {"Direct funds transfer",
        fun credit_account/0},
       {"Process a credit card",
        fun process_card/0},
       {"Return the corresponding URL for the resource",
        fun get_rsc_endpoint/0},
       {"Check the current application mode",
        fun debug_mode/0},
       {"Check the API access keys",
        fun api_keys/0}
     ]
     }
    }.


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
%%% tests
%%%-------------------------------------------------------------------
create_invoice() ->
     ?assertEqual(1, 1).
confirm_invoice() ->
     ?assertEqual(1, 1).
create_opr() ->
     ?assertEqual(1, 1).
charge_opr() ->
     ?assertEqual(1, 1).
credit_account() ->
     ?assertEqual(1, 1).
process_card() ->             
    ?assertEqual(1, 1).
get_rsc_endpoint() ->             
    ?assertEqual(1, 1).
debug_mode() ->             
    ?assertEqual(1, 1).
api_keys() ->             
    ?assertEqual(1, 1).
