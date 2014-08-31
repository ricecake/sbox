-module(sbox_shell).

%% api
-export([listen/1, listen/2]).

%% %% our shell function
%-export([start_shell/1]).

listen(Port) ->
    listen(Port, [{system_dir, "/home/ricecake/Projects/sbox/ssh"}]).

listen(Port, Options) ->
    ssh:daemon(any, Port, [{pwdfun, fun(User, Pass)-> true end}, {shell, fun(U, H) -> start_shell(U, H) end} | Options]).

%%% spawns out shell loop, we use plain io to input and output
%%% over ssh (the group module is our group leader, and takes
%%% care of sending input to the ssh_sample_cli server)
start_shell(User, Peer) ->
    spawn(fun() ->
		  io:format("Enter command\n"),
		  shell_loop({User, Peer})
	  end).

%%% an ordinary Read-Eval-Print-loop
shell_loop({User, Peer}) ->
    % Read
    Line = io:get_line("CLI> "),
    % Eval
    Result = eval_cli(Line),
    % Print
    io:format("---> ~p\n", [Result]),
    case Result of
	done -> 
	    exit(normal);
	_ -> 
	    shell_loop({User, Peer})
    end.

eval_cli(Line) -> sbox_command_tokenizer:line(Line).
