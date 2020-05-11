-module(jsonerl).

-export([encode/1, decode/1, encode_tojson/1, decode_fromjson/1]).

-include_lib("eunit/include/eunit.hrl").

encode_tojson(Anything) ->
    try
	jsone:encode(encode(Anything))
    catch
	error:badarg ->
	    throw({invalid_json, Anything})
    end.

encode(E) when is_atom(E) ->
    #{<<"t">> => <<"atom">>, <<"v">> => list_to_binary(atom_to_list(E))};
encode(E) when is_tuple(E) ->
    #{<<"t">> => <<"tuple">>, <<"v">> => encode(tuple_to_list(E))};
encode(E) when is_map(E) ->
    #{<<"t">> => <<"map">>, <<"v">> => encode(maps:to_list(E))};
encode(E) when is_binary(E) ->
    ensure_utf8(E);
encode(E) when is_number(E) ->
    E;
encode([E | Rest]) ->
    [encode(E) | encode(Rest)];
encode([]) ->
    [];
encode(V) ->
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


decode_fromjson(Anything) ->
    try
	decode(jsone:decode(Anything))
    catch
	error:badarg ->
	    throw({invalid_json, Anything})
    end.

decode(#{<<"t">> := <<"atom">>, <<"v">> := V}) ->
    binary_to_existing_atom(V, utf8);
decode(#{<<"t">> := <<"tuple">>, <<"v">> := V}) ->
    list_to_tuple(decode(V));
decode(#{<<"t">> := <<"map">>, <<"v">> := V}) ->
    maps:from_list(decode(V));
decode(E) when is_binary(E); is_number(E) ->
    E;
decode([E | Rest]) ->
    [decode(E) | decode(Rest)];
decode([]) ->
    [];
decode(V) ->
    throw({invalid_json, V}).

-ifdef(TEST).

encode_decode_maps_1() ->
    [{1, 1}, {a, #{<<"t">> => <<"atom">>, <<"v">> => <<"a">>}},
     {{1,2}, #{<<"t">> => <<"tuple">>, <<"v">> => [1, 2]}},
     {#{<<"a">> => 1},
      #{<<"t">> => <<"map">>,
	<<"v">> => [#{<<"t">> => <<"tuple">>, <<"v">> => [<<"a">>, 1]}]}}
    ].

encode_test_() ->
    lists:map(fun({V, T}) ->
		      %?debugFmt("~p~n", [encode(V)]),
		      ?_assert(encode(V) =:= T)
	      end, encode_decode_maps_1()).

decode_test_() ->
    lists:map(fun({V, T}) ->
		      ?_assert(decode(T) =:= V)
	      end, encode_decode_maps_1()).

encode_decode_maps_2() ->
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

encode_tojson_test_() ->
    lists:map(fun({V, T}) ->
		      %?debugFmt("~p~n", [encode_tojson(V)]),
		      ?_assert(encode_tojson(V) =:= T)
	      end, encode_decode_maps_2()).

encode_tojson_invliad_utf8_test() ->
    ?assert(encode_tojson(<<192>>) =:= <<"\"non-utf8\"">>).

decode_fromjson_test_() ->
    lists:map(fun({V, T}) ->
		      ?_assert(decode_fromjson(T) =:= V)
	      end, encode_decode_maps_2()).

-endif.

