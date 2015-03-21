% client abstracts communication with the server
% just publics a user friendly api, not messages details
% The essence of the problem has to do with creating parallel processes and sending and receiving messages
% it's important to define which messages and their content

-module(afile_client).
-author({joe,armstrong}).
-export([ls/1, get_file/2, put_file/3, rm/2]).

ls(Server) ->
    Server ! {self(), list_dir},
    receive
        {Server, FileList} ->
            FileList
    end.

get_file(Server, File) ->
    Server ! {self(), {get_file, File}},
    receive
        {Server, Content} ->
            Content
    end.

put_file(Server, File, Content) ->
    Server ! {self(), {put_file, File, Content}},
    receive
        {Server, Result} ->
            Result
    end.

rm(Server, File) ->
    Server ! {self(), {delete, File}},
    receive
        {Server, Result} ->
            Result
    end.
