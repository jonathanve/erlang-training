% a module will always run b's newest version
% Behaviour:
% process already started will run with the version they started,
% even if there is a new version. (this applies to a module)
% Just 2 Versions:
% Erlang can have two versions of a module running at any one time, 
% the current version and an old version.
% this means there may be processes running the old version.

-module(a).
-compile(export_all).

start(Tag) ->
    spawn(fun() -> loop(Tag) end).

loop(Tag) ->
    sleep(),
    Val = b:x(),
    io:format("Vsn3 (~p) b:x() = ~p~n", [Tag, Val]),
    loop(Tag).

sleep() ->
    receive
	after 2000 -> true
    end.
