%%%-------------------------------------------------------------------
%%% @author jonathanve <jonathanve@vm>
%%% @copyright (C) 2015, jonathanve
%%% @doc
%%%
%%% @end
%%% Created : 16 Apr 2015 by jonathanve <jonathanve@vm>
%%%-------------------------------------------------------------------
-module(simple_logger).

-behaviour(gen_event).

%% API
-export([start/0, stop/0, log/1, report/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%% Adds current module as default event handler
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    case gen_event:start_link({local, ?SERVER}) of
	R = {ok, _Pid} ->
	    gen_event:add_handler(?SERVER,?MODULE,arg1),
	    R;
	Other ->
	    Other
    end.

stop() ->
    gen_event:stop(?SERVER).

log(E) ->
    gen_event:notify(?SERVER, {log, E}).
report() ->
    gen_event:call(?SERVER, ?MODULE, report).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(arg1) ->
    io:format("Starting event manager~n"),
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({log, E}, State) -> {ok, trim([E|State])}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(report, State) -> {ok, State, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(stop, _) -> true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
trim([X1,X2,X3,X4,X5|_]) -> [X1,X2,X3,X4,X5];
trim(L) -> L.
