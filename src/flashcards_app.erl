-module(flashcards_app).
-behaviour(application).

-export([start/0]).
-export([start/2]).
-export([stop/1]).
-export([start_phase/3]).

start() ->
	ensure_app(cowboy),
	application:start(flashcards).

ensure_app(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error,{not_started,Dep}} ->
			ensure_app(Dep),
			ensure_app(App);
		{error,{already_started,App}} ->
			ok
	end.
start(normal, _Args) ->
	io:format("===> FlashCard app starting...~n", []),
	flashcards_sup:start_link().

stop(_State) ->
	io:format("===< FlashCard app stopped.~n", []),
	ok.
%% Simple Cowboy http routing.
%% From example at
%% https://github.com/extend/cowboy/tree/master/examples/hello_world
start_phase(listen, _Type, _Args) ->

	io:format("===> Starting handler.~n", []),
	Dispatch = cowboy_router:compile(
		[{'_', [
		% {"/",                    handle_root,   []}
			%% @todo dummy hello user for now.
			{"/hello",              handle_user,  []}
			]}
		]),

	cowboy:start_http(http, 100,
		[{port, config(flashcards, http_port)}],
		[{env, [{dispatch, Dispatch}]}]),
	ok.

config(App, Key) ->
	case application:get_env(App, Key) of
		undefined -> erlang:error({missing_config, Key});
		{ok, Val} -> Val
	end.

