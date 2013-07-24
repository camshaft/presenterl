-module (presenterl_test).

-include_lib ("eunit/include/eunit.hrl").

-define (SIMPLE_EXPECTED, <<"{\"hello\":\"world\",\"testing\":123,\"this is a test\":{\"href\":\"http://example.com\"},\"boolean\":true}">>).

-define (NUM_TESTS, 50000).

no_encoder_test() ->
  P = presenterl:create(),

  Value = [
    {<<"testing">>, 123}
  ],

  P ! Value,

  ActualValue = presenterl:encode(P),

  ?assertEqual(Value, ActualValue).

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

  ActualValue = presenterl:encode(P),

  ?assertEqual(?SIMPLE_EXPECTED, ActualValue),

  {error, closed} = presenterl:encode(P).

conditional_test() ->
  P = presenterl:create(jsx),

  presenterl:conditional(true, [{<<"boolean">>, true}], P),

  presenterl:conditional(false, [{<<"boolean">>, false}], P),

  presenterl:conditional(true, fun() ->
    [
      {<<"fun">>, true}
    ]
  end, P),

  ActualValue = presenterl:encode(P),

  ?assertEqual(<<"{\"boolean\":true,\"fun\":true}">>, ActualValue).

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
  Presenterl = time(presenterl_test, 0, ?NUM_TESTS),
  Traditional = time(normal_test, 0, ?NUM_TESTS),

  ?debugVal(Presenterl / Traditional).

time(_, Acc, 0) ->
  Acc;
time(F, Acc, Num) ->
  {Result, _} = timer:tc(?MODULE, F, []),
  time(F, Acc + Result, Num - 1).
