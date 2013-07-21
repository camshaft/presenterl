-module(presenterl).

-export([create/0]).
-export([create/1]).
-export([create/2]).
-export([loop/4]).

-export([add/2]).
-export([concat/2]).
-export([conditional/3]).
-export([encode/1]).

create() ->
  spawn_link(?MODULE, loop, [undefined, [], undefined, self()]).

create(Serializer) ->
  spawn_link(?MODULE, loop, [Serializer, [], undefined, self()]).

create(Serializer, Options) ->
  spawn_link(?MODULE, loop, [Serializer, [], Options, self()]).

add(Data, Presenter) ->
  Presenter ! {add, Data},
  ok.

concat(Data, Presenter) ->
  Presenter ! Data,
  ok.

conditional(true, Data, Presenter) ->
  Presenter ! Data,
  ok;
conditional(Conditionals, Data, Presenter) when is_list(Conditionals) ->
  case check(Conditionals) of
    true ->
      Presenter ! Data;
    _ ->
      ok
  end,
  ok;
conditional(_, _, _) ->
  ok.

check([]) ->
  true;
check([true|Rest]) ->
  check(Rest);
check(_) ->
  false.

encode(Presenter) ->
  case process_info(Presenter) of
    undefined ->
      %% TODO should we save the result somewhere and return it?
      {error, closed};
    _ ->
      Presenter ! {encode, self()},
      receive
        {ok, Presenter, Out} ->
          Out;
        Message ->
          Message
      end
  end.

loop(Serializer, Body, Options, Owner) ->
  receive
    {encode, Owner} ->
      Out = case Options of
        undefined ->
          case Serializer of
            undefined ->
              Body;
            Serializer ->
              Serializer:encode(Body)
          end;
        _ ->
          Serializer:encode(Body, Options)
      end,

      Owner ! {ok, self(), Out};
    {encode, Caller} ->
      Caller ! {error, not_owner};
    {add, Data} ->
      ?MODULE:loop(Serializer, [Data|Body], Options, Owner);
    Data ->
      ?MODULE:loop(Serializer, Body ++ Data, Options, Owner)
  end.
