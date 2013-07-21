presenterl
===========

Simple presenter for erlang

Why?
----

Have you ever tried constructing JSON responses with a bunch of conditional branches?

```erlang
Body = [],

Body2 = case is_authorized(<<"username">>, Req) of
  true ->
    [
      {<<"username">>, <<"CamShaft">>}
    ] ++ Body;
  false ->
    Body
end,

Body3 = case acl_lookup(<<"admin">>, Req) of
  true ->
    case is_authorized(<<"email">>, Req) of
      true ->
        [
          {<<"email">>, <<"cameron@theflokk.com">>}
        ] ++ Body2;
      false ->
        Body2
    end;
  false ->
    Body2
end,

%% ... snip

Body47 = case is_authorized(<<"last-name">>, Req) of
  true ->
    [
      {<<"last-name">>, <<"Bytheway">>}
    ] ++ Body46;
  false ->
    Body46
end,

JSON = jsx:encode(Body47).
```

Not fun. This is where presenterl helps.

Usage
-----

```erlang
P = presenterl:create(jsx),

presenterl:conditional(is_authorized(<<"email">>, Req), [
  {<<"username">>, <<"CamShaft">>}
], P),

presenterl:conditional([
  acl_lookup(<<"admin">>, Req),
  is_authorized(<<"email">>, Req)
], [
  {<<"email">>, <<"cameron@theflokk.com">>}
], P),

%% ... snip

presenterl:conditional(is_authorized(<<"last-name">>, Req), [
  {<<"last-name">>, <<"Bytheway">>}
], P),

JSON = presenterl:encode(P).
```

API
---

`TODO` - Read the tests and code for now.

Performance
-----------

Right now presenterl is about half the speed of the less convenient way of building JSON representations. Given that the difference averages about 40us on my MacBook Pro it's probably not that big of a deal considering there are bigger bottlenecks than that.

Tests
-----

```sh
make test
```
