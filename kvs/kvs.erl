-module(kvs).
-export([start/0, lookup/1, store/2]).

start() ->
    register(kvs, spawn(fun() -> loop() end)).

lookup(Key) ->
    rpc({lookup, Key}).

store(Key, Value) ->
    rpc({store, Key, Value}).

rpc(Q) ->
    kvs ! {self(), Q},
    receive
	{kvs, Reply} ->
	    Reply
    end.

loop() ->
    receive
	{From, {store, Key, Value}} ->
		put(Key, {ok, Value}),
		From ! {kvs, true},
		loop();
	{From, {lookup, Key}} ->
		From ! {kvs, get(Key)},
		loop()
    end.
