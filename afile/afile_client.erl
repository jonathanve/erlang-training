% client abstracts communication with the server
% just publics a user friendly api, not messages details
% The essence of the problem has to do with creating parallel processes and sending and receiving messages
% it's important to define which messages and their content

-module(afile_client).
-author({joe,armstrong}).
-export([ls/1, get_file/2, put_file/3, rm/2]).

ls(Server) ->
    rpc(Server, list_dir).

get_file(Server, File) ->
    rpc(Server, {get_file, File}).

put_file(Server, File, Content) ->
    rpc(Server, {put_file, File, Content}).

rm(Server, File) ->
    rpc(Server, {delete, File}).

-spec rpc(Pid, Request) -> Response when
	Pid      :: pid(),
	Request  :: term(),
	Response :: term().

rpc(Pid, Request) -> 
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.
