%
% creating modules, high order functions, list comprehensions
%

-module(shop).
-export([cost/1, total/1, total_payment/1]).
-import(lists, [sum/1, map/2]).

cost(oranges)   -> 5;
cost(newspaper) -> 8;
cost(apples)    -> 2;
cost(pears)     -> 9;
cost(milk)      -> 7.

total(L) ->
    sum(map(fun({What, N}) -> shop:cost(What) * N end, L)).

total_payment(L) ->
    sum([shop:cost(What) * N || {What, N} <- L]).
