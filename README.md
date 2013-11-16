MPower Payments Erlang client library
=====================================

This is a minimal Erlang client library for [MPower Payments](http://mpowerpayments.com).
This library currently supports the following operations:

* Create invoice
* Confirm Invoice
* OPR create and charge
* Direct card processing/billing through MPower Payments
* Direct funds transfer(direct pay) through MPower Payments

## setup
````bash
$ git clone https://github.com/mawuli-ypa/mpower-erlang
$ cd mpower-erlang; make; make eunit # to run eunit tests
$ OR rebar get-deps; rebar compile; rebar eunit skip_deps=true #if you use the rebar build tool
````

## Usage
Start an Erlang shell and type the following:
````erlang
Erlang R16B (erts-5.10.1) [source] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> application:start(mpower).
%% debug mode is set to false by default so all API requests
%% go to the LIVE server by default. To run in debug mode, do:
2> application:set_env(mpower, debug, true).
3> API_KEYS = [{master_key, "YOUR MPower MASTER KEY HERE"},
             {private_key, "YOUR MPower PRIVATE KEY"},
             {token, "Your MPower token here"}].
4> application:set_env(mpower, api_keys, API_KEYS).
5> Token = "test_ghytae". %% invoice token
6> mpower:confirm_invoice("Token").
{mpower_response,true,<<"00">>,<<"Transaction Found">>,
                 [{<<"actions">>,
                   {[{<<"cancel_url">>,null},{<<"return_url">>,null}]}},
                  {<<"invoice">>,
                   {[{<<"items">>,
                      {[{<<"item_21">>,
                         {[{<<"description">>,<<"VIP Tickets for the MPower Event">>},
                           {<<"total_price">>,<<"70.0">>},
                           {<<"name">>,<<"VIP Ticket">>},
                           {<<"unit_price">>,<<"35.0">>},
                           {<<"quantity">>,2}]}},
                        {<<"item_20">>,
                         {[{<<"description">>,<<"VIP Tickets for the MPower E"...>>},
                           {<<"total_price">>,<<"70.0">>},
                           {<<"name">>,<<"VIP Ticket">>},
                           {<<"unit_price">>,<<"35.0">>},
                           {<<"quantity">>,2}]}},
                        {<<"item_22">>,
                         {[{<<"description">>,<<"VIP Tickets for the MPow"...>>},
                           {<<"total_price">>,<<"70.0">>},
                           {<<"name">>,<<"VIP Ticket">>},
                           {<<"unit_price">>,<<"35.0">>},
                           {<<"quantity">>,2}]}},
                        {<<"item_14">>,
                         {[{<<"description">>,<<"VIP Tickets for the "...>>},
                           {<<"total_price">>,<<"70.0">>},
                           {<<"name">>,<<"VIP Ticket">>},
                           {<<"unit_price">>,<<"35.0">>},
                           {<<"quantity">>,2}]}},
                        {<<"item_15">>,
                         {[{<<"description">>,<<"VIP Tickets for "...>>},
                           {<<"total_price">>,<<"70.0">>},
                           {<<"name">>,<<"VIP Tick"...>>},
                           {<<"unit_pri"...>>,<<"35.0">>},
                           {<<"quan"...>>,2}]}},
                        {<<"item_16">>,
                         {[{<<"description">>,<<"VIP Tickets "...>>},
                           {<<"total_price">>,<<"70.0">>},
                           {<<"name">>,<<"VIP "...>>},
                           {<<"unit"...>>,<<...>>},
                           {<<...>>,...}]}},
                        {<<"item_17">>,
                         {[{<<"description">>,<<"VIP Tick"...>>},
                           {<<"total_pr"...>>,<<"70.0">>},
                           {<<"name">>,<<...>>},
                           {<<...>>,...},
                           {...}]}},
                        {<<"item_10">>,
                         {[{<<"descript"...>>,<<"VIP "...>>},
                           {<<"tota"...>>,<<...>>},
                           {<<...>>,...},
                           {...}|...]}},
                        {<<"item_11">>,
                         {[{<<"desc"...>>,<<...>>},{<<...>>,...},{...}|...]}},
                        {<<"item_12">>,{[{<<...>>,...},{...}|...]}},
                        {<<"item_13">>,{[{...}|...]}},
                        {<<"item_18">>,{[...]}},
                        {<<"item"...>>,{...}},
                        {<<...>>,...},
                        {...}|...]}},
                     {<<"total_amount">>,1610.0},
                     {<<"taxes">>,{[]}},
                     {<<"description">>,null}]}},
                  {<<"custom_data">>,{[]}},
                  {<<"mode">>,<<"test">>},
                  {<<"status">>,<<"pending">>}],
                 200}
7> rr(mpower). %% load records from the `mpower` module
[mpower_directcard,mpower_directpay,mpower_invoice,
 mpower_opr,mpower_response,mpower_store]
8> Transfer = #mpower_directpay{account_alias="02XXXXXXXX", amount=100}.
9> mpower:credit_account(Transfer). %% You can also pass around records :)
#mpower_response{success = false,code = <<"2001">>,
                 text = <<"MPower::Exception, An Error Occured on the server.">>,
                 data = [],http_status = 200}
10> Card = [{card_name, "Alfred Robert Rowe"},
        {card_number, 4242424242424242}, {card_cvc, 123},
        {exp_month,06}, {exp_year ,2010}, {amount ,300}]
11> mpower:process_card(Card).
#mpower_response{success = false,code = <<"3001">>,
                 text = <<"Authentication Error! Account credentials are invalid. Try again">>,
                 data = [],http_status = 200}
12> mpower:charge_opr(<<"test_ghabyd">>, <<"9606">>). %% charge_opr(Token, UserSubmittedToken).
{mpower_response,false,<<"1001">>,
                 <<"MPower::Integration::Exception, OPR Token is invalid, no request records found.">>,
                 [],200}
````

See the `test/mpower_test.erl` file for more examples on how to use the library
 in your application.


## Note
* All API calls/actions  return an #mpower_response{} record.
This makes it easy for consumption of the returned data, diagnosing errors etc.
* Most of the **CREATE** function calls accept either a record or a proplist
* Uses [lhttpc](https://github.com/esl/lhttpc) for the HTTP requests.


## LICENSE
MIT LICENSE(see LICENSE.txt)


## Authors
Mawuli Adzaku <mawuli@mawuli.me>
