%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2013, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 15 Nov 2013 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(mpower).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include("mpower.hrl").
%% MPower Payments API calls
-export([opr_create/1,
         opr_charge/2,        
         invoice_create/1,
         invoice_confirm/1,
         direct_pay_credit_account/1,
         direct_card_process/1
        ]).

-export([request_headers/0,
        debug_mode/0,
        get_rsc_endpoint/1]).


%%%-------------------------------------------------------------------
%%% Invoice
%%%-------------------------------------------------------------------
%% @doc Create a new invoice
-spec invoice_create(Invoice) -> http_response() when
      Invoice :: #mpower_invoice{} | proplist().
invoice_create(Invoice) ->
    ok.

%% @doc Returns the status of the invoice
-spec invoice_confirm(Token :: mpower_token()) -> http_response().
invoice_confirm(Token) ->
    ok.


%%%-------------------------------------------------------------------
%%% Direct payments: direct card and direct pay
%%%-------------------------------------------------------------------
%% @doc Directly transfer funds between MPower accounts
-spec direct_pay_credit_account(Transaction) -> http_response() when
      Transaction :: #mpower_directpay{} | proplist().
direct_pay_credit_account(Transaction) ->
    ok.

%% @doc Directly bill/charge a credit through MPower 
-spec direct_card_process(Card) -> http_response() when
      Card :: #mpower_directcard{} | proplist().
direct_card_process(Card) ->
    ok.

%%%-------------------------------------------------------------------
%%% OPR
%%%-------------------------------------------------------------------
%% @doc initiate an OPR(Onsite Payment Request)
-spec opr_create(Data) -> http_response() when
      Data ::  #mpower_opr{} | proplist().
opr_create(Data) ->
    ok.

%% @doc Second and final stage of an OPR process.
%%      This completes the OPR.
-spec opr_charge(OPRToken, ConfirmationToken) -> http_response() when
      OPRToken :: mpower_token(),
      ConfirmationToken :: mpower_token().
opr_charge(OPRToken, ConfirmationToken) ->
    ok.

%%%-------------------------------------------------------------------
%%% utility functions
%%%-------------------------------------------------------------------
-spec get_rsc_endpoint(Resource) -> URL when
      Resource :: atom(),
      URL :: string().
get_rsc_endpoint(opr_create)->
    api_endpoint() ++ "opr/create";
get_rsc_endpoint(opr_charge) ->
    api_endpoint() ++ "opr/charge";
get_rsc_endpoint(invoice_create) ->
    api_endpoint() ++ "checkout-invoice/create";
get_rsc_endpoint(invoice_confirm) ->
    api_endpoint() ++ "checkout-invoice/confirm/";
get_rsc_endpoint(directcard_process) ->
    api_endpoint() ++ "direct-card/processcard";
get_rsc_endpoint(directpay_process) ->
    api_endpoint() ++ "direct-pay/credit-account";
get_rsc_endpoint(_) ->
    undefined.

%% @doc Returns the corresponding API endoint for the current mode
-spec api_endpoint() -> string().
api_endpoint() ->
    case debug_mode() of
        true -> 
            ?SANDBOX_ENDPOINT;
         _ -> 
            ?LIVE_ENDPOINT
    end.

%% @doc returns the application's current mode
-spec debug_mode() -> true | false.
debug_mode() ->
    case application:get_env(?APP_NAME, debug) of 
        {ok, true} ->
            true;
        _ ->
            false
    end.

%% @doc make an HTTP GET request
-spec get(Url :: string()) -> http_response().
get(Url) ->
    {ok, {{_, StatusCode, _}, _, Body}} =  httpc:request(get, {Url, request_headers(),
                                                                "application/json",
                                                                []}, [], []
                                                        ),
    {StatusCode == 200, jiffy:decode(Body)}.

%% @doc send an HTTP POST request
%% R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
%% {ok, {{"HTTP/1.1",ReturnCode, State}, Head, Body}} = R.
-spec post(Url :: string(), Data :: [tuple()]) -> http_response().
post(Url, Data) ->
    {ok, {{_, StatusCode, _}, _, Body}} =  httpc:request(post, {Url, request_headers(),
                                                                "application/json",
                                                                jiffy:encode(Data)}, [], []
                                                        ),
    {StatusCode == 200, jiffy:decode(Body)}.


%% Construct the HTTP request headers to be sent to the MPower Payments server
-spec request_headers() -> [tuple()].
request_headers() ->
    case application:get_env(?APP_NAME, api_keys) of
        {ok, API_KEYS} when is_list(API_KEYS) ->
            [
             {"MP-Master-Key", proplists:get_value(master_key, API_KEYS)},
             {"MP-Public-Key", proplists:get_value(public_key, API_KEYS)},
             {"MP-Token", proplists:get_value(token, API_KEYS)},              
             {"User-Agent", ?MP_USER_AGENT}
            ];
        _ ->
            []
    end.
