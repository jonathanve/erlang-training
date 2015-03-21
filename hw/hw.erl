% 
% fault-tolerance, concurrency, distributed, non-stop
%
% world is concurrent
% things do not share resources
% communication via messages
% things fail
% functional programming, dynamic typing encourages creativity
% substitution model easy to test
% tail-recursive algorithms for speed
% pattern matching for clarity
% concurrency has to do with software; parallelism with hardware.
% 
% unix philosophy (large small programs and pipes)
% a process is the unit of abstraction
% process are independent and isolated
% an erlang process is a little vm
% communication only by message passing
% process share no resources
% process do what they are supposed to do or fail.
% processes can run on different machines
% if you know the name, you can send a message to import
% error handling non-local (remote error detection)
% modelling concurrency (identify concurrent processes)
% otp: supervisor, gen_server, gen_event, gen_fsm
% integers, floats, atoms, tuples, lists, funs, list comprehensions, records, maps
% q(). % quit and close    f(). % unbound variables
% list comprehensions are expressive but they can be tough to understand at first
% guards: (; % OR) (, % AND)
% functions can be refered: fun local_fun/arity fun mod:remote_fun/arity
% include files (hrl)(-include) and macros (?MACRO)(-define)
% 

-module(hw).
-export([hello_world/0, fac/1, fib/1, area/1, areas/1,
         sum/1, pythag/1, perms/1, map_f/2, filter/2,
         split/1]).
-import(lists, [map/2]).

hello_world() -> io:fwrite("hello, world\n").

sum(L) when is_list(L) -> sum_r(L, 0).

sum_r([], X)    -> X;
sum_r([H|T], X) -> sum_r(T, H + X).

fac(N) when is_integer(N), N >= 0 -> fac_r(N, 1).

fac_r(0, Acc) -> Acc;
fac_r(N, Acc) -> fac_r(N - 1, N * Acc).

fib(N) when is_integer(N), N >= 0 -> fib_r(N, 0, 1).

fib_r(0, _, _) -> 0;
fib_r(1, _, Y) -> Y;
fib_r(N, X, Y) -> fib_r(N - 1, Y, X + Y).

areas(L) ->
    lists:sum(
              map(
                  fun(I) -> area(I) end,
                  L)).

area({square, Side})             -> Side * Side;
area({circle, Radius})           -> 3.14150 * Radius * Radius;
area({rectangle, Width, Height}) -> Width * Height.

map_f(_, [])    -> [];
map_f(F, [H|T]) -> [F(H)|map(F, T)].

filter(_, [])    -> [];
filter(P, [H|T]) -> 
    case P(H) of
	true  -> [H|filter(P, T)];
	false -> filter(P, T)
    end.


% uses a list comprehension to calculate the pythagorean triplets
pythag(N) ->
    [ {A, B, C} ||
	A <- lists:seq(1, N),
	B <- lists:seq(1, N),
	C <- lists:seq(1, N),
	A + B + C =< N,
	A * A + B * B =:= C * C
    ].

% anagrams
% it is better to build this from right to left (<-)
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

% accumularor pattern
split(L) ->
    split_acc(L, [], []).

split_acc([H|T], Evens, Odds) ->
    case (H rem 2) of
	    0 -> split_acc(T, [H|Evens], Odds);
	    1 -> split_acc(T, Evens, [H|Odds])
    end;
split_acc([], Evens, Odds) ->
    {lists:reverse(Evens), lists:reverse(Odds)}.
