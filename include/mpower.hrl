%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2013, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 15 Nov 2013 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% types
%%%-------------------------------------------------------------------
-type response_code() :: integer().
-type response_text() :: binary().
-type response_data() :: binary().
-type invoice_status() :: pending | cancelled | completed.
-type mpower_account() :: pos_integer().
-type proplist()     :: [{term(), term()}].  % predefined in newer releases
-type http_response() :: {true | false, json()}.
-type mpower_token() :: string().
-type amount() :: pos_integer().
-type tax() :: {string(), amount()}.
-type item() :: {string(), amount()}.
-type json() :: tuple().

%%%-------------------------------------------------------------------
%%% records
%%%-------------------------------------------------------------------
-record(mpower_store, {name :: string(),
                       tagline :: string(),
                       postal_address :: string(),
                       phone :: string(),
                       logo_url :: string(),
                       website_url :: string()
                      }).

-record(mpower_directpay, {account_alias :: mpower_account(),
                           amount :: amount()
                          }).
-record(mpower_directcard, {card_name :: string(),
                            card_number :: integer(),
                            card_cvc :: integer(),
                            exp_month :: integer(),
                            exp_year :: integer(),
                            amount :: amount()
                           }).
-record(mpower_invoice, {items :: [item()],
                         taxes :: [tax()],
                         total_amount :: amount(),
                         description :: string(),
                         store :: #mpower_store{},
                         custom_data :: [tuple()],
                         actions :: [tuple()] %% [c{ancel_url, foo}, {return_url, bar}]
                        }).
-record(mpower_opr, {account_alias :: mpower_account(),
                     description :: string(),
                     total_amount :: amount(),
                     store :: #mpower_store{}
                    }).
-record(mpower_response, {success :: true | false,
                          code :: response_code(), 
                          text :: response_text(), 
                          data :: response_data(),
                          http_status :: integer()
                         }).

%%%-------------------------------------------------------------------
%%% miscellaneous
%%%-------------------------------------------------------------------
%% api keys used for testing
-define(API_VERSION, "v1").
-define(MPOWER_TEST_API_KEYS, [{master_key, "5b9f531a-fbb8-487a-8045-3b4c7ac5acee"},
                         {private_key, "test_private_oGslgmzSNL3RSkjlsnPOsZZg9IA"},
                         {token, "ff1d576409b2587cc1c2"}
                         ]). 
%% Sandbox Endpoint
-define(SANDBOX_ENDPOINT, "https://app.mpowerpayments.com/sandbox-api/" ++ ?API_VERSION ++ "/").

%% Live Endpoint
-define(LIVE_ENDPOINT, "https://app.mpowerpayments.com/api/" ++ ?API_VERSION ++ "/").

%% user-agent headers
-define(MP_USER_AGENT, "mpower-erlang/v0.1.0").

-define(APP_NAME, mpower).
%%timeout for HTTP requests
-define(HTTP_TIMEOUT, 10000).
%% All successful MPower requests have "00" as the response code
-define(MPOWER_API_SUCCESS_CODE, <<"00">>).

%% Turns a record into a proplist
-define(R2P(Record,RecordType), lists:zip(record_info(fields, RecordType), tl(tuple_to_list(Record)))).
