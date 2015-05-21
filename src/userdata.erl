-module(userdata).

-export([load/1, stats/1, update/2, choose_card/1, get_data/0]).

%% UserData - your data structure to represent everything we know about the user.
%% {C, W, LastCard, FileName}
%% , where C is number of correct answers, and W is number of wrong answers.
%% And LastCard is either 'nothing' or the card, like "C".
%% And FileName is the name of the user's file to persist the user data.

load(_FileName) ->
	read_data("foo"). % TODO: fix hardcoded name "foo"

% stats(Data) -> Status.
stats({C, W, _LastCard, _FileName}) ->
	{C, W}.

% update(Data, Event) -> NewData.
update({C, W, _LastCard, FileName}, {AddC, AddW, Card}) ->
	Data = {C + AddC, W + AddW, Card, FileName},
	write_data(FileName, Data),
	Data.

% choose_card(Data) -> Card.
choose_card({_, _, LastCard, _FileName}) ->
	%% This is where the possible choices are currently stored
	Cards = ["A","B", "C", "D", "E","F","G"],
	choose_cards2([C || C <- Cards, C =/= LastCard]).

choose_cards2(Cards) ->
	% Here we randomly choose a card out of Cards.
	lists:nth(crypto:rand_uniform(0, length(Cards))+1, Cards).

get_data() ->
	read_data("foo"). % TODO: fix hardcoded name "foo"


read_data(FileName) ->
	% if file FileName already exists then
	%     read userdata from the file
	% else
	%     initialize userdata
	%     write userdata to file
	case file:read_file_info(FileName) of
		{ok, _} ->
			{ok, Binary} = file:read_file(FileName),
			binary_to_term(Binary);
		_ ->
			Data = {0, 0, nothing, FileName},
			file:write_file(FileName, term_to_binary(Data)),
			Data
	end.

write_data(FileName, Data) ->
	file:write_file(FileName, term_to_binary(Data)).
