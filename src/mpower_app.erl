%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2013, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 15 Nov 2013 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(mpower_app).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-behaviour(application).
-vsn("0.1.0").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = ensure_started(inets),
    ok = ensure_started(crypto),
    ok = ensure_started(public_key),
    ok = ensure_started(ssl),
    ok = ensure_started(lhttpc),
    mpower_sup:start_link().

stop(_State) ->
    ok.

%% @doc Ensure that the application is started
-spec ensure_started(App) -> ok | {error, already_started, App} when
      App :: atom().
ensure_started(App) ->
    case application:start(App) of 
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
     end.
