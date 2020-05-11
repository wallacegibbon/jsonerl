-module(jsonerl).

-export([encode/1, decode/1]).

-include_lib("eunit/include/eunit.hrl").

encode(Anything) ->
    try
	jsone:encode(encode_1(Anything))
    catch
	error:badarg ->
	    throw({invalid_json, Anything})
    end.

encode_1(E) when is_atom(E) ->
    #{<<"t">> => <<"atom">>, <<"v">> => list_to_binary(atom_to_list(E))};
encode_1(E) when is_tuple(E) ->
    #{<<"t">> => <<"tuple">>, <<"v">> => encode_1(tuple_to_list(E))};
encode_1(E) when is_map(E) ->
    #{<<"t">> => <<"map">>, <<"v">> => encode_1(maps:to_list(E))};
encode_1(E) when is_binary(E) ->
    ensure_utf8(E);
encode_1(E) when is_number(E) ->
    E;
encode_1([E | Rest]) ->
    [encode_1(E) | encode_1(Rest)];
encode_1([]) ->
    [];
encode_1(V) ->
    throw({invalid_json, V}).

%% erlang binary could be NOT utf8
%% @TODO: use a better way to check valid utf8 string
ensure_utf8(Binary) when is_binary(Binary) ->
    try
	jsone:encode(Binary),
	Binary
    catch
	error:badarg ->
	    <<"non-utf8">>
    end.


decode(Anything) ->
    try
	decode_1(jsone:decode(Anything))
    catch
	error:badarg ->
	    throw({invalid_json, Anything})
    end.

decode_1(#{<<"t">> := <<"atom">>, <<"v">> := V}) ->
    binary_to_existing_atom(V, utf8);
decode_1(#{<<"t">> := <<"tuple">>, <<"v">> := V}) ->
    list_to_tuple(decode_1(V));
decode_1(#{<<"t">> := <<"map">>, <<"v">> := V}) ->
    maps:from_list(decode_1(V));
decode_1(E) when is_binary(E); is_number(E) ->
    E;
decode_1([E | Rest]) ->
    [decode_1(E) | decode_1(Rest)];
decode_1([]) ->
    [];
decode_1(V) ->
    throw({invalid_json, V}).

-ifdef(TEST).

encode_decode_maps() ->
    [{1, <<"1">>},
     {a, <<"{\"t\":\"atom\",\"v\":\"a\"}">>},
     {<<"ab">>, <<"\"ab\"">>},
     {{1,2}, <<"{\"t\":\"tuple\",\"v\":[1,2]}">>},
     {[1,2], <<"[1,2]">>},
     {[1,[2,[3]]], <<"[1,[2,[3]]]">>},
     {[1,[2,[<<"blah">>]]], <<"[1,[2,[\"blah\"]]]">>},
     {#{}, <<"{\"t\":\"map\",\"v\":[]}">>},
     {#{a => 1, b => 2}, <<"{\"t\":\"map\",\"v\":[{\"t\":\"tuple\",\"v\":[{\"t\":\"atom\",\"v\":\"a\"},1]},{\"t\":\"tuple\",\"v\":[{\"t\":\"atom\",\"v\":\"b\"},2]}]}">>},
     {#{a => #{b => 2}}, <<"{\"t\":\"map\",\"v\":[{\"t\":\"tuple\",\"v\":[{\"t\":\"atom\",\"v\":\"a\"},{\"t\":\"map\",\"v\":[{\"t\":\"tuple\",\"v\":[{\"t\":\"atom\",\"v\":\"b\"},2]}]}]}]}">>}
    ].

encode_test_() ->
    lists:map(fun({V, T}) ->
		      %?debugFmt("~p~n", [encode(V)]),
		      ?_assert(encode(V) =:= T)
	      end, encode_decode_maps()).

encode_invliad_utf8_test() ->
    ?assert(encode(<<192>>) =:= <<"\"non-utf8\"">>).

decode_test_() ->
    lists:map(fun({V, T}) ->
		      ?_assert(decode(T) =:= V)
	      end, encode_decode_maps()).

-endif.

