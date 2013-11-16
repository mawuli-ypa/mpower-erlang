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
    Res = mpower:charge_opr(<<"test_ghabyd">>, <<"9606">>),
    %% test for successful HTTP request
    ?assertEqual(Res#mpower_response.http_status, 200),
    %% HTTP request might have succceeded but PAI call/operation failed
    ?assertEqual(Res#mpower_response.success, false),
    ?assertNotEqual(Res#mpower_response.code, ?MPOWER_API_SUCCESS_CODE).
credit_account() ->
     ?assertEqual(1, 1).
process_card() ->             
    Card = [{card_name, <<"Alfred Robert Rowe">>},
            {card_number, <<"4242424242424242">>}, 
            {card_cvc, <<"123">>},
            {exp_month, <<"06">>}, 
            {exp_year, <<"2010">>}, 
            {amount, <<"300">>}
           ],
    Res = mpower:process_card(Card),
    %% this should fail since the card is invalid
    ?assertEqual(Res#mpower_response.success, false).
get_rsc_endpoint() ->
    Url = mpower:get_rsc_endpoint(opr_charge),
    ?assertEqual(true, string:str(Url, "opr/charge") > 0),
    Url1 = mpower:get_rsc_endpoint(opr_create),
    ?assertEqual(true, string:str(Url1, "opr/create") > 0),
    Url2 = mpower:get_rsc_endpoint(invoice_confirm),
    ?assertEqual(true, string:str(Url2, "checkout-invoice/confirm") > 0),
    Url3 = mpower:get_rsc_endpoint(invoice_create),
    ?assertEqual(true, string:str(Url3, "sandbox") > 0),
    Url4 = mpower:get_rsc_endpoint(directcard_process),
    ?assertEqual(true, string:str(Url4, "https") > 0),
    Url5 = mpower:get_rsc_endpoint(directpay_process),
    ?assertEqual(true, string:str(Url5, ?API_VERSION) > 0).

debug_mode() ->
    {ok, State} = application:get_env(mpower, debug),
    ?assertEqual(State, true).
api_keys() ->             
    {ok, API_KEYS} = application:get_env(mpower, api_keys),
    ?assertEqual(API_KEYS, ?MPOWER_TEST_API_KEYS).
