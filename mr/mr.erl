% records are tuples (fixed number of atoms)
% maps are just key-value pairs (ohashes or dicts)
% The best way to use maps is to always use Key => Val the first time a key is defined and use Key := Val each time the value of a specific key is changed.
% #{ name => jonathan, age => 25 } map declaration
% #{ name := Name} = Map

-module(mr).
-export([clear_status/1, do_something/1, count_chars/1]).
-include("records.hrl").

clear_status(#todo{status=_, who=_} = R) ->
    R#todo{status=finished}.

do_something(X) when is_record(X, todo) ->
    do_something.

% map example
% count_chars(Str) ->
%     count_chars(Str, #{}).

% count_chars([H|T], #{ H := N } = X) ->
%     count_chars(T, X#{ H := N + 1 });
% count_chars([H|T], X) ->
%     count_chars(T, X#{ H => 1 });
% count_chars([], X) ->
%     X.
