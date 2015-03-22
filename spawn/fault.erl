-module(fault).
-export([on_exit/2]).

on_exit(Pid, Fun) ->
    spawn(fun() ->
		  Ref = monitor(process, Pid),
		  receive
		      {'DOWN', Ref, process, Pid, Why} ->
			  Fun(Why)
		  end
	  end).
