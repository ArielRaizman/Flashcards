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
	W = 0,
	C = 0,
	reply_dtl(Req, user_dtl, [
		% parameters like {card, Value}
		{wrong, W},
		{correct, C},
		{card, "B"}
	]).

handle_post(Req) ->
	{ok, KeyVals, Req2} = cowboy_req:body_qs(Req, []),
	{C, W} = get_status(KeyVals),
	{AddC, AddW} = get_res(KeyVals),
	NewW = W + AddW,
	NewC = C + AddC,
	reply_dtl(Req2, user_dtl, [
		% parameters like {card, Value}
		{wrong, NewW},
		{correct, NewC},
		{card, "B"}
	]).

get_status(KeyVals) ->
	get_status(
		lists:keyfind(<<"c">>, 1, KeyVals),
		lists:keyfind(<<"w">>, 1, KeyVals)).
get_status({_, C},{_, W}) -> {to_num(C), to_num(W)}.

get_res(KeyVals) ->
	get_res(
		lists:keyfind(<<"b">>, 1, KeyVals),
		lists:keyfind(<<"d">>, 1, KeyVals)).
get_res(false, {_, _D}) -> {0, 1};
get_res({_, _B}, false) -> {1, 0}.

to_num(B) when is_binary(B) -> to_num(binary_to_list(B));
to_num(L) when is_list(L)   -> list_to_integer(L);
to_num(N)                   -> N.


reply_dtl(Req, Template, Params) ->
	{ok, Content} = Template:render(Params),
	cowboy_req:reply(200, [{<<"Content-Type">>,<<"text/html">>}], Content, Req).

