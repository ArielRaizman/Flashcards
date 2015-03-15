-module(handle_user).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
%	io:format("===> Request '/hello': ~p~n", [Req]),
	case cowboy_req:method(Req) of
		{<<"GET">>, Req2} ->
			erlang:insert_element(3,
				cowboy_req:reply(200,
					[{<<"Content-Type">>, <<"text/html">>}],
					 <<"Hello...">>, Req2),
				State);

		{<<"POST">>, Req2} ->
			erlang:insert_element(3,
				cowboy_req:reply(200,
					[{<<"Content-Type">>, <<"text/html">>}],
					 <<"Hello...">>, Req2),
				State)
	end.

terminate(_Reason, _Req, _State) ->
	ok.
