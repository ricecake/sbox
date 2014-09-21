-module(sbox_shell).

%% api
-export([listen/1, listen/2, start_shell/2]).

%% %% our shell function
%-export([start_shell/1]).

listen(Port) ->
    listen(Port, [{system_dir, "/home/ricecake/Projects/sbox/ssh"}]).

listen(Port, Options) ->
	ssh:daemon(Port, [{pwdfun, fun(User, Pass)->
		{ok, Auth} = sbox_auth_srv:auth(User, Pass),
		Auth
	end}, {shell, fun(U, H) -> start_shell(U, H) end} | Options]).

%%% spawns out shell loop, we use plain io to input and output
%%% over ssh (the group module is our group leader, and takes
%%% care of sending input to the ssh_sample_cli server)
start_shell(User, Peer) ->
	{ok, Auth} = sbox_auth_srv:fetch(User),
	ok = sbox_auth_srv:expire(User),
	spawn(fun() ->
		io:format("Enter command\n"),
		shell_loop({User, Peer}, Auth)
	end).

%%% an ordinary Read-Eval-Print-loop
shell_loop({User, {IP, _Port} = Peer}, Auth) ->
    % Read
    Line = io:get_line(io_lib:format("~s@~s >> ", [User, inet_parse:ntoa(IP)])),
    % Eval
    Result = eval_cli(Line, Auth),
    % Print
    io:format("---> ~p\n", [Result]),
    case Result of
	done -> 
	    exit(normal);
	_ -> 
	    shell_loop({User, Peer}, Auth)
    end.

eval_cli(Line, _Auth) ->
	{ok, Tokens, _} = sbox_command_tokenizer:string(Line),
	process(sbox_command_parser:parse(Tokens)).


process({Command, _Details}) -> {unknown, Command}.
