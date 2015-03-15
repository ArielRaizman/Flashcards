-module(handle_user).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
%	io:format("===> Request '/hello': ~p~n", [Req]),
	case cowboy_req:method(Req) of
		{<<"GET">>, Req2} ->
			erlang:insert_element(3, handle_get(Req2), State);
		{<<"POST">>, Req2} ->
			erlang:insert_element(3, handle_post(Req2), State)
	end.

terminate(_Reason, _Req, _State) ->
	ok.

handle_get(Req) ->
	reply_dtl(Req, user_dtl, [
		% parameters like {card, Value}
		]).

handle_post(Req) ->
	reply_dtl(Req, user_dtl, [
		% parameters like {card, Value}
		]).


reply_dtl(Req, Template, Params) ->
	{ok, Content} = Template:render(Params),
	cowboy_req:reply(200, [{<<"Content-Type">>,<<"text/html">>}], Content, Req).

