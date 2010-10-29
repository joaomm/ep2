%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(chat_client).

-import(io_widget, 
	[get_state/1, insert_str/2, set_prompt/2, set_state/2, 
	 set_title/2, set_handler/2, update_state/3, set_group_list/2]).

-export([start/0, test/0, connect/5]).


start() -> 
    connect("localhost", 2223, "AsDT67aQ", "general", "joe").


test() ->
    connect("localhost", 2223, "AsDT67aQ", "general", "joe"),
    connect("localhost", 2223, "AsDT67aQ", "general", "jane"),
    connect("localhost", 2223, "AsDT67aQ", "general", "jim"),
    connect("localhost", 2223, "AsDT67aQ", "general", "sue").
	   

connect(Host, Port, HostPsw, Group, Nickname) ->
    spawn(fun() -> start_client(Host, Port, HostPsw, Group, Nickname) end).
				 
start_client(Host, Port, HostPsw, Group, Nickname) ->
    process_flag(trap_exit, true),
	Widget = create_widget(Nickname),
    start_connection(Host, Port, HostPsw),    
    main_loop(Widget, Group, Nickname).

create_widget(Nickname) ->
    Widget = io_widget:start(self()),
    set_title(Widget, Nickname),
    set_state(Widget, Nickname),
    set_prompt(Widget, [Nickname, " > "]),
    set_handler(Widget, fun parse_command/1),
	Widget.

start_connection(Host, Port, Pwd) ->
    S = self(),
    spawn_link(fun() -> connection_loop(S, Host, Port, Pwd) end).

connection_loop(Parent, Host, Port, Pwd) ->
    %% Parent is the Pid of the process that spawned this process
    case lib_chan:connect(Host, Port, chat, Pwd, []) of
	{error, _Why} ->
	    Parent ! {status, {cannot, connect, Host, Port}},
	    sleep(2000),
	    connection_loop(Parent, Host, Port, Pwd);
	{ok, MM} ->
	    lib_chan_mm:controller(MM, Parent),
	    Parent ! {connected, MM},
	    exit(connectorFinished)
    end.

main_loop(Widget, Group, Nickname) ->
    receive
	{connected, MM} ->
	    login(MM, Widget, Group, Nickname);
	{Widget, destroyed} ->
	    exit(died);
	{status, S} ->
		io:format("Aqui com ~p: ~p~n", [Nickname, S]),
	    insert_str(Widget, to_str(S)),
	    main_loop(Widget, Group, Nickname);
	Other ->
	    io:format("chat_client disconnected unexpected:~p~n",[Other]),
	    main_loop(Widget, Group, Nickname)
    end.

login(MM, Widget, Group, Nickname) ->
	insert_str(Widget, "connected to server\nsending data\n"),
    lib_chan_mm:send(MM, {login, Group, Nickname}),
    wait_login_response(Widget, MM).

wait_login_response(Widget, MM) ->
    receive
	{chan, MM, ack} ->
	    active_loop(Widget, MM);
	Other ->
	    io:format("chat_client login unexpected:~p~n",[Other]),
	    wait_login_response(Widget, MM)
    end. 

active_loop(Widget, MM) ->
     receive
	 {Widget, Nick, Str} ->
	     lib_chan_mm:send(MM, {relay, Nick, Str}),
	     active_loop(Widget, MM);
	 {chan, MM, {msg, From, Pid, Str}} ->
	     insert_str(Widget, [From,"@",pid_to_list(Pid)," ", Str, "\n"]),
	     active_loop(Widget, MM);
	 {chan, MM, {group_list, GroupList}} ->
		set_group_list(Widget, GroupList),
		active_loop(Widget, MM);
	 {'EXIT',Widget,windowDestroyed} ->
	     lib_chan_mm:close(MM);
	 {close, MM} ->
	     exit(serverDied);
	 Other ->
	     io:format("chat_client active unexpected:~p~n",[Other]),
	     active_loop(Widget, MM)
     end. 

sleep(T) ->
    receive
    after T -> true
    end.
	    
to_str(Term) ->
    io_lib:format("~p~n",[Term]).

parse_command(Str) -> skip_to_gt(Str).

skip_to_gt(">" ++ T) -> T;
skip_to_gt([_|T])    -> skip_to_gt(T);
skip_to_gt([])       -> exit("no >").
