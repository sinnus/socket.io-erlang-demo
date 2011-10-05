-module(common).
-export([to_record/3]).
%% TODO Extxract to common,hrl???
-include("demo.hrl").

%% -record(test_record,
%% 	{key1,
%% 	 key2,
%% 	 key3}).

%% main() ->
%%     ?TERM_TO_RECORD(test_record, [{<<"key1">>, <<"value1">>},
%% 				  {<<"key2">>, <<"value2">>},
%% 				  {<<"key3">>, 0},
%% 				  {<<"key4">>, undefined}]). 

to_record(Values, Fallback, Fields) ->
    list_to_tuple([element(1, Fallback) | decode_record_fields(Values, Fallback, 2, Fields)]).

decode_record_fields(_Values, _Fallback, _Index, []) ->
    [];
decode_record_fields(Values, Fallback, Index, [Field | Rest]) ->
    [case lists:keysearch(list_to_binary(atom_to_list(Field)), 1, Values) of
	 {value, {_, Value}} ->
	     Value;
	 false ->
	     element(Index, Fallback)
     end | decode_record_fields(Values, Fallback, Index + 1, Rest)].
