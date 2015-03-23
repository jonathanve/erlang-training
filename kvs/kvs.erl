% concurrency, fault tolerance, distributed
% same primitives but node must be specified
% i.e: spawn(Node, Fun)
% epmd is used to connect erl nodes
% erl -name <name> -setcookie <cookie>
% {RegName, Node} ! Msg (send message Msg to process RegName in Node)
% new processes can be started in remote nodes
% the set of connected nodes having the same cookie defines an Erlang cluster
% lib_chan is a module that allows a user to control which processes are spawned on their machines.
% socket based distribution is used when we want to improve security at expense of flexibility

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

