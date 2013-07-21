-module (presenterl_test).

-include_lib ("eunit/include/eunit.hrl").

-define (SIMPLE_EXPECTED, <<"{\"hello\":\"world\",\"testing\":123,\"this is a test\":{\"href\":\"http://example.com\"},\"boolean\":true}">>).

-define (NUM_TESTS, 50000).

simple_test() ->
  P = presenterl:create(jsx),

  P ! [
    {<<"hello">>, <<"world">>},
    {<<"testing">>, 123}
  ],

  P ! [
    {<<"this is a test">>, [
      {<<"href">>, <<"http://example.com">>}
    ]},
    {<<"boolean">>, true}
  ],

  {ok, Out} = presenterl:encode(P),

  ?assertEqual(?SIMPLE_EXPECTED, Out),

  {error, closed} = presenterl:encode(P),

  Out.

conditional_test() ->
  P = presenterl:create(jsx),

  presenterl:conditional(true, [{<<"boolean">>, true}], P),

  presenterl:conditional(false, [{<<"boolean">>, false}], P),

  {ok, Out} = presenterl:encode(P),

  ?assertEqual(<<"{\"boolean\":true}">>, Out).

presenterl_test() ->
  P = presenterl:create(jsx),

  P ! [
    {<<"hello">>, <<"world">>},
    {<<"testing">>, 123}
  ],

  P ! [
    {<<"this is a test">>, [
      {<<"href">>, <<"http://example.com">>}
    ]},
    {<<"boolean">>, true}
  ],

  presenterl:encode(P).

normal_test() ->
  Body = [
    {<<"hello">>, <<"world">>},
    {<<"testing">>, 123}
  ],

  Body2 = Body ++ [
    {<<"this is a test">>, [
      {<<"href">>, <<"http://example.com">>}
    ]},
    {<<"boolean">>, true}
  ],

  jsx:encode(Body2).

timing_test() ->
  Result = time(presenterl_test, 0, ?NUM_TESTS),
  Result2 = time(normal_test, 0, ?NUM_TESTS),

  0 = Result / Result2.

time(_, Acc, 0) ->
  Acc;
time(F, Acc, Num) ->
  {Result, _} = timer:tc(?MODULE, F, []),
  time(F, Acc + Result, Num - 1).
