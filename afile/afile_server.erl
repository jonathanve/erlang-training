% I learned erlang uses the unix philosophy
% concurrency: software, paralelism: hardware
% erlc to compile, erl to run shell and programs
% primitives: spawn, send, and receive
% each process has its own private data (process dict s8.21)

-module(afile_server).
-author({joe,armstrong}).
-export([start/1, loop/1]).

start(Dir) ->
    spawn(?MODULE, loop, [Dir]).

loop(Dir) ->
    receive
        {Client, list_dir} ->
            Client ! {self(), file:list_dir(Dir)};
        {Client, {get_file, File}} ->
            Full = filename:join(Dir, File),
            Client ! {self(), file:read_file(Full)};
        {Client, {put_file, File, Content}} ->
            Filename_Put = filename:join(Dir, File),
            Res = file:write_file(Filename_Put, Content),
            Client ! {self(), Res};
        {Client, {delete, File}} ->
            Filename_Delete = filename:join(Dir, File),
            Client ! {self(), file:delete(Filename_Delete)}
    end,
    loop(Dir).
