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
-export([create_opr/1,
         charge_opr/2,        
         create_invoice/1,
         confirm_invoice/1,
         credit_account/1,
         process_card/1
        ]).

-export([request_headers/0,
        debug_mode/0,
        get_rsc_endpoint/1]).


%%%-------------------------------------------------------------------
%%% Invoice
%%%-------------------------------------------------------------------
%% @doc Create a new invoice
-spec create_invoice(Invoice) -> http_response() when
      Invoice :: #mpower_invoice{} | proplist().
create_invoice(Invoice) when is_record(Invoice, mpower_invoice) ->
    ok;
create_invoice(Invoice) ->
    ok.

%% @doc Returns the status of the invoice
-spec confirm_invoice(Token :: mpower_token()) -> http_response().
confirm_invoice(Token) ->
    Url = get_rsc_endpoint(invoice_confirm) ++ Token,
    Res = http_get(Url),
    Res.

%%%-------------------------------------------------------------------
%%% Direct payments: direct card and direct pay
%%%-------------------------------------------------------------------
%% @doc Directly transfer funds between MPower accounts
-spec credit_account(Transaction) -> http_response() when
      Transaction :: #mpower_directpay{} | proplist().
credit_account(Transaction) when is_record(Transaction, mpower_directpay)->
    Url = get_rsc_endpoint(directpay_process),
    Res = http_post(Url, [{account_alias, Transaction#mpower_directpay.account_alias},
                          {amount, Transaction#mpower_directpay.amount}
                         ]),
    Res;
credit_account(Transaction) ->
    Url = get_rsc_endpoint(directpay_process),
    Res = http_post(Url, Transaction),
    Res.

%% @doc Directly bill/charge a credit through MPower 
-spec process_card(Card) -> http_response() when
      Card :: #mpower_directcard{} | proplist().
process_card(Card) when is_record(Card, mpower_directcard) ->
    Url = get_rsc_endpoint(directpay_process),
    Res = http_post(Url, [{card_name, Card#mpower_directcard.card_name},
                          {card_number, Card#mpower_directcard.card_number},
                          {card_cvc, Card#mpower_directcard.card_cvc},
                          {exp_month, Card#mpower_directcard.exp_month},
                          {exp_year, Card#mpower_directcard.exp_year},
                          {amount, Card#mpower_directcard.amount}
                         ]),
    Res;
process_card(Card) ->
    Url = get_rsc_endpoint(directpay_process),
    Res = http_post(Url, Card),
    Res.

%%%-------------------------------------------------------------------
%%% OPR
%%%-------------------------------------------------------------------
%% @doc initiate an OPR(Onsite Payment Request)
-spec create_opr(Data) -> http_response() when
      Data ::  #mpower_opr{} | proplist().
create_opr(Data)  when is_record(Data, mpower_opr) ->
    ok;
create_opr(Data) ->
    ok.

%% @doc Second and final stage of an OPR process.
%%      This completes the OPR.
-spec charge_opr(OPRToken, ConfirmationToken) -> http_response() when
      OPRToken :: mpower_token(),
      ConfirmationToken :: mpower_token().
charge_opr(OPRToken, ConfirmationToken) ->
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
-spec http_get(Url :: string()) -> http_response().
http_get(Url) ->
    {ok, {{StatusCode, _}, _, Json}} = lhttpc:request(Url, "GET",
                                                      request_headers(),
                                                      ?HTTP_TIMEOUT
                                                     ),
    parse_api_response(StatusCode, Json).

%% @doc send an HTTP POST request
-spec http_post(Url :: string(), Body :: [tuple()]) -> http_response().
http_post(Url, Body) ->
    {ok, {{StatusCode, _}, _, Json}} = lhttpc:request(Url, "POST",
                                                      request_headers(),
                                                      jiffy:encode({Body}),
                                                      ?HTTP_TIMEOUT
                                                     ),
    parse_api_response(StatusCode, Json).


%% Construct the HTTP request headers to be sent to the MPower Payments server
-spec request_headers() -> [tuple()].
request_headers() ->
    case application:get_env(?APP_NAME, api_keys) of
        {ok, API_KEYS} when is_list(API_KEYS) ->
            [
             {"MP-Master-Key", proplists:get_value(master_key, API_KEYS)},
             {"MP-Private-Key", proplists:get_value(private_key, API_KEYS)},
             {"MP-Token", proplists:get_value(token, API_KEYS)},              
             {"User-Agent", ?MP_USER_AGENT},
             {"Content-Type", "application/json"}
            ];
        _ ->
            [{"User-Agent", ?MP_USER_AGENT},
             {"Content-Type", "application/json"}
            ]
    end.


%% @doc Parses response from the MPower server.
%%      The parsed data is an #mpower_response
-spec parse_api_response(StatusCode, Body) -> #mpower_response{} when
      StatusCode :: integer(),
      Body :: json().
parse_api_response(200, Body) ->
    {Code, Text, Json} = decompose_response_body(Body),
    #mpower_response{success = Code == ?MPOWER_API_SUCCESS_CODE, 
                     code=Code, text=Text, data=Json, 
                     http_status=200};
parse_api_response(HttpStatusCode, Body) ->
    {Code, Text, Json} = decompose_response_body(Body),
    #mpower_response{success = Code == ?MPOWER_API_SUCCESS_CODE, 
                     code=Code, text=Text, data=Json, 
                     http_status=HttpStatusCode}.    

%% @doc Decomposes the server response into 
%%  reuseable information.
-spec decompose_response_body(Body) -> {Code, Text, Json} when
      Body :: json(),
      Code :: binary(),
      Text :: binary(),
      Json :: proplist().
decompose_response_body(Body) ->
    {Json} = jiffy:decode(Body),
    [Code] = jsonq:q([<<"response_code">>], Json),
    [Text] = jsonq:q([<<"response_text">>], Json),
    Json2 = proplists:delete(<<"response_code">>, Json),
    Json3 = proplists:delete(<<"response_text">>, Json2),
    {Code, Text, Json3}.
