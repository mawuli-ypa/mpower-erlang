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
-define(MPOWER_STORE_NAME, "FooBar Dot Erl").
-define(MPOWER_ACC_ALIAS, <<"0266636984">>).


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
        fun api_keys/0},
       {"Format [{k,v}, ..] taxes into json encodable forms",
        fun reformat_taxes/0}
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
    Res = create_dummy_invoice(),
    ?assertEqual(true, Res#mpower_response.success),

    %% check for existence of invoice token
    %% json:q returns [MatchedKeys]
    Found = jsonq:q([<<"token">>], Res#mpower_response.data),
    ?assertNotEqual([], Found).

confirm_invoice() ->
    Res = create_dummy_invoice(),
    [Token] = jsonq:q([<<"token">>], Res#mpower_response.data),
    Res1 = mpower:confirm_invoice(Token),
    ?assertEqual(true, Res1#mpower_response.success),
    
    Token1 = "test_InvalidToken", %% invalid invoice token 
    Res2 = mpower:confirm_invoice(Token1),
    ?assertEqual(false, Res2#mpower_response.success).

create_opr() ->
    Store = #mpower_store{name=?MPOWER_STORE_NAME, 
                          website_url="http://foobar.erl"},
    OPR = #mpower_opr{total_amount = <<"450">>,
                     description = <<"MPower Erlang Test OPR">>,
                     store=Store,
                     account_alias=?MPOWER_ACC_ALIAS},
    Res = mpower:create_opr(OPR),
    ?assertEqual(true, Res#mpower_response.success).

charge_opr() ->
    Res = mpower:charge_opr(<<"test_ghabyd">>, <<"9606">>),
    %% test for successful HTTP request
    ?assertEqual(200, Res#mpower_response.http_status),
    %% HTTP request might have succceeded but API call/operation failed
    ?assertEqual(false, Res#mpower_response.success),
    ?assertNotEqual(?MPOWER_API_SUCCESS_CODE, Res#mpower_response.code).

credit_account() ->
    Transaction = [{amount, 230},
                   {account_alias, ?MPOWER_ACC_ALIAS}],
    Res = mpower:credit_account(Transaction),
    ?assertEqual(true, Res#mpower_response.success).

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
    ?assertEqual(false, Res#mpower_response.success).

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
    ?assertEqual(true, mpower:debug_mode()).

api_keys() ->             
    {ok, API_KEYS} = application:get_env(mpower, api_keys),
    ?assertEqual(?MPOWER_TEST_API_KEYS, API_KEYS).

reformat_taxes() ->
    Taxes = [{nhis_tax, 20},{vat, 15},{ama, 78}],
    {ExpectedCount, ExpectedTaxes} = {3, [{<<"tax_0">>, {[{nhis_tax,20}]}},
                                          {<<"tax_1">>, {[{vat,15}]}},
                                          {<<"tax_2">>, {[{ama,78}]}}
                                         ]
                                     },
    {Count, FormattedTaxes} = mpower:reformat_taxes(Taxes),
    ?assertEqual(ExpectedCount, Count),
    ?assertEqual(ExpectedTaxes, FormattedTaxes).


%%%------------------------------------------------------------------------
%%% private functions used within this module only, and excluded from tests
%%%------------------------------------------------------------------------
create_dummy_invoice() ->
     Store = [{name, <<"FooBar Dot Erl">>},
              {website_url,<<"http://foobar.erl">>}],
     Taxes = [{nhis_tax, 20},{vat, 15},{ama, 78}],
     Items = [{<<"OKoK Men's shoes">>, 130}, 
              {<<"Vegetarian Pizza">>, 40}
             ],
     CustomData = [{voucher_key, 8765567988}, {order_prio, 1}],
    TotalAmount = 123,
    Description = <<"Sample Invoice">>,
    Actions = [{return_url, <<"http://foobar.erl/mpower_return_url">>},
               {cancel_url, <<"http://foobar.erl/mpower_cancel_url">>}],
    Invoice=[{total_amount, TotalAmount},
             {description, Description},
             {taxes, Taxes},
             {items, Items},
             {actions, Actions}, 
             {custom_data, CustomData}, 
             {store, Store}],
    Res = mpower:create_invoice(Invoice),
    Res.
