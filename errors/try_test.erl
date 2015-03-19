% use cases:
% use error(Why) to improve error messages
% return values such us {ok, ...}, {error, Why}

-module(try_test).
-export([demo1/0, demo2/0, read/1]).

generate_exception(1) ->
    a;
generate_exception(2) ->
    throw(a);
generate_exception(3) ->
    exit(a);
generate_exception(4) ->
    {'EXIT', a};
generate_exception(5) ->
    error(a).

% using try...catch
catcher(N) ->
    try generate_exception(N) of
	Val -> {N, normal, Val}
    catch
	throw:X -> {N, caught, thrown, X};
	exit:X  -> {N, caught, exited, X};
	error:X -> {N, caught, error, X}
    end.

demo1() ->
    [catcher(I) || I <- [1, 2, 3, 4, 5]].

% using just catch
demo2() ->
    [{I, (catch generate_exception(I))} || I <- [1, 2, 3, 4, 5]].

read(File) ->
    case file:read_file(File) of
	{ok, Bin}    -> Bin;
	{error, Why} -> error(Why)
    end.
