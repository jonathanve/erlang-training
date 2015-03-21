% client-server model
% primitives: spawn, send, receive

-module(area_server).
-export([start/0, area/2, loop/0]).

start() ->
    spawn(?MODULE, loop, []).

area(Pid, What) ->
    rpc(Pid, What).

% this will take max 5 secs to respond
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    after 5000 ->
        {error, timeout}
    end.

loop() ->
    receive
	{From, {rectangle, Width, Height}} ->
	    From ! {self(), Width * Height};
	{From, {circle, R}} ->
	    From ! {self(), 3.14159 * R * R};
	{From, Other} ->
            sleep(10000),
	    From ! {self(), {error, Other}}
    end,
    loop().

sleep(T) ->
    receive
    after T ->
        true
    end.
