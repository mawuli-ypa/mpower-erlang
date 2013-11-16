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
$ OR rebar get-deps; rebar compile; rebar eunit  #if you use the rebar build tool
````

## Usage
Start an Erlang shell and type the following:
````erlang
1> application:start(mpower).
%% debug mode is set to false by default so all API requests
%% go to the LIVE server by default. To run in debug mode, do:
2> application:set_env(mpower, debug, true).
3> API_KEYS = [{master_key, "YOUR MPower MASTER KEY HERE"},
             {public_key, "YOUR MPower PUBLIC KEY"},
             {token, "Your MPower token here"}],
4> application:set_env(mpower, api_keys, API_KEYS).
````

See the `test/mpower_test.erl` file for examples of how to use the library
 in your application.


## LICENSE
MIT LICENSE(see LICENSE.txt)


## Authors
Mawuli Adzaku <mawuli@mawuli.me>
