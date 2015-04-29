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
			{ok, KeyVals, Req3} = cowboy_req:body_qs(Req2, []),
			% Is it answer or question dun dun dun...
			case lists:keyfind(<<"answer">>, 1, KeyVals) of
				false ->
					% User just gave answer for a card! display is it correct or wrong.
					erlang:insert_element(3, handle_answer(Req3, KeyVals), State);
				_ ->
					% We just shown user correct answer! display next card.
					erlang:insert_element(3, handle_question(Req3, KeyVals), State)
			end
	end.

terminate(_Reason, _Req, _State) ->
	ok.

handle_get(Req) ->
	W = 0,
	C = 0,
	Card = choose_card(),
	reply_dtl(Req, user_dtl, [
		{wrong, W},
		{correct, C},
		{card, notesvg:name_to_svg(Card ++ "4")},
		{cardletter, Card},
		{cardoctave, 4},
		{cardstate, "normal"}
	]).

handle_answer(Req, KeyVals) ->
	{C, W} = get_status(KeyVals),
	{AddC, AddW, Card} = get_res(KeyVals),
	NewW = W + AddW,
	NewC = C + AddC,
	reply_dtl(Req, user_dtl, [
		{wrong, NewW},
		{correct, NewC},
		{card, notesvg:name_to_svg(Card ++"4")},
		{cardletter, Card},
		{cardoctave, 4},
		{cardstate, get_state(AddC, AddW)},
		{disabled, "disabled"}
	]).

handle_question(Req, KeyVals) ->
	{C, W} = get_status(KeyVals),
	{_, _, Card} = get_res(KeyVals),
	Card2 = choose_card(Card),
	reply_dtl(Req, user_dtl, [
		{wrong, W},
		{correct, C},
		{card, notesvg:name_to_svg(Card2 ++"4")},
		{cardletter, Card2},
		{cardoctave, 4},
		{cardstate, "normal"}
	]).

get_state(1, 0) -> "correct";
get_state(0, 0) -> "normal";
get_state(0, 1) -> "wrong".


get_status(KeyVals) ->
	get_status(
		lists:keyfind(<<"c">>, 1, KeyVals),
		lists:keyfind(<<"w">>, 1, KeyVals)).
get_status({_, C},{_, W}) -> {to_num(C), to_num(W)};
get_status(_, _) -> {0, 0}.

get_res(KeyVals) ->
	get_res(
		lists:keyfind(<<"choice">>, 1, KeyVals),
		lists:keyfind(<<"card">>, 1, KeyVals)).
get_res({_, Card},    {_, Card}) -> {1,0, binary_to_list(Card)};
get_res({_, _Choice}, {_, Card}) -> {0,1, binary_to_list(Card)};
get_res( _,           {_, Card}) -> {0,0, binary_to_list(Card)}.

to_num(B) when is_binary(B) -> to_num(binary_to_list(B));
to_num(L) when is_list(L)   -> list_to_integer(L);
to_num(N)                   -> N.

choose_card() ->
	%% This is where the possible choices are currently stored
	Cards = ["A","B", "C", "D", "E","F","G"],
	choose_cards2(Cards).

choose_card(Card) ->
	%% This is where the possible choices are currently stored
	Cards = ["A","B", "C", "D", "E","F","G"],
	choose_cards2([C || C <- Cards, C =/= Card]).

choose_cards2(Cards) ->
	% Here we randomly choose a card out of Cards.
	lists:nth(crypto:rand_uniform(0, length(Cards))+1, Cards).

reply_dtl(Req, Template, Params) ->
	{ok, Content} = Template:render(Params),
	cowboy_req:reply(200, [{<<"Content-Type">>,<<"text/html">>}], Content, Req).
