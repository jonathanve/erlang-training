% apps: typer, dialyzer
% typer: gives info about used types
% dialyzer: finds discrepancies in erl programs
% Advices:
% 1.  think about the types and declare them
% 2.  write the specs of your functions
% 3.  start implementing and use dialyzer
% 4.  fix all types errors or logic
% 5.  implement 1 function at a time
% 6.  don't -compile(export_all).
% 7.  detail type specs for exported functions
% 8.  provide dafault args to all elements in a record def.
% 9.  typer and dialyzer not enough to create quality software
% 10. use test cases as well

-module(test1).
-export([f1/0]).

f1() ->
    X = erlang:time(),
    seconds(X).

seconds({Hour, Min, Sec}) ->
    (Hour * 60 + Min) * 60 + Sec.
