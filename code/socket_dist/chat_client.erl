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
	 set_title/2, set_handler/2, update_state/3, set_group_list/3, set_groups/2, destroy/1]).

-export([start/0, start/1, test/0, connect/4]).


start() -> connect("localhost", 2223, "AsDT67aQ", "joe").
start(Nickname) -> connect("localhost", 2223, "AsDT67aQ", Nickname).

test() ->
    connect("localhost", 2223, "AsDT67aQ", "joe"),
    connect("localhost", 2223, "AsDT67aQ", "jane"),
    connect("localhost", 2223, "AsDT67aQ", "jim"),
    connect("localhost", 2223, "AsDT67aQ", "sue").
	   

connect(Host, Port, HostPsw, Nickname) ->
    spawn(fun() -> start_client(Host, Port, HostPsw, Nickname) end).
				 
start_client(Host, Port, HostPsw, Nickname) ->
    process_flag(trap_exit, true),
    start_connection(Host, Port, HostPsw),
	main_loop(Nickname).

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

main_loop(Nickname) ->
    receive
	{connected, MM} ->
	    get_group_list(MM, Nickname);
	{status, _S} ->
	    main_loop(Nickname);
	Other ->
	    io:format("chat_client disconnected unexpected:~p~n",[Other]),
	    main_loop(Nickname)
    end.

get_group_list(MM, Nickname) ->
	lib_chan_mm:send(MM, grouplist),
	receive
		{chan, MM, GroupList} ->
			io:format("RECEBI a LISTA ~p ~n", [GroupList]),
			start_group_list(MM, GroupList, Nickname)
	end.

start_group_list(MM, GroupList, Nickname) ->
	GroupListWidget = create_group_list_widget(GroupList, Nickname),
	group_list_loop(MM, Nickname, GroupListWidget).

create_group_list_widget(GroupList, Nickname) ->
	Widget = io_widget:start(self()),
    set_title(Widget, "GroupList " ++ Nickname),
    set_prompt(Widget, [" > "]),
    set_handler(Widget, fun parse_group/1),
	set_groups(Widget, GroupList),
	Widget.

group_list_loop(MM, Nickname, GroupListWidget) ->
	receive
	{_Widget, {group, Group}} ->
		login(MM, Group, Nickname, GroupListWidget);
	Other ->
		io:format("chat_client login unexpected:~p~n",[Other]),
		group_list_loop(MM, Nickname, GroupListWidget)
	end.

login(MM, Group, Nickname, GroupListWidget) ->
	destroy(GroupListWidget),
	ChatWidget = create_chat_widget(Nickname, Group),
    lib_chan_mm:send(MM, {login, Group, Nickname, node()}),
	wait_login_response(ChatWidget, MM, Nickname).

create_chat_widget(Nickname, Group) ->
    Widget = io_widget:start(self()),
	set_title(Widget, Nickname ++ "@" ++ Group),
    set_state(Widget, Nickname),
    set_prompt(Widget, [Nickname, " > "]),
    set_handler(Widget, fun parse_command/1),
	Widget.

wait_login_response(Widget, MM, Nickname) ->
    receive
	{chan, MM, ack} ->
	    active_loop(Widget, MM, Nickname);
	Other ->
	    io:format("chat_client login unexpected:~p~n",[Other]),
	    wait_login_response(Widget, MM, Nickname)
    end. 

active_loop(Widget, MM, Nickname) ->
     receive
	 {Widget, Nick, Str} ->
	     lib_chan_mm:send(MM, {relay, Nick, Str}),
	     active_loop(Widget, MM, Nickname);
	
	 {Widget, {to, Destinatary, Message}} ->
		lib_chan_mm:send(MM, {to, Nickname, Destinatary, Message}),
		active_loop(Widget, MM, Nickname);
		
	 {chan, MM, {msg, From, Pid, Str}} ->
	     insert_str(Widget, [From,"@",pid_to_list(Pid)," ", Str, "\n"]),
	     active_loop(Widget, MM, Nickname);
	
	 {chan, MM, {direct_msg, From, Pid, Str}} ->
		 insert_str(Widget, ["DirectMsg ",From,"@",pid_to_list(Pid)," ", Str, "\n"]),
		 active_loop(Widget, MM, Nickname);
	
	 {chan, MM, {group_list, GroupName, GroupList}} ->
		set_group_list(Widget, GroupName, GroupList),
		active_loop(Widget, MM, Nickname);
	 {'EXIT',Widget,windowDestroyed} ->
	    lib_chan_mm:close(MM),
		start(Nickname);
	 {close, MM} ->
	     exit(serverDied);
	 Other ->
	     io:format("chat_client active unexpected:~p~n",[Other]),
	     active_loop(Widget, MM, Nickname)
     end. 

sleep(T) ->
    receive
    after T -> true
    end.
	    
to_str(Term) ->
    io_lib:format("~p~n",[Term]).

parse_command(Str) -> 
	StrWithoutGt = skip_to_gt(Str),
	parse_command_and_messages(StrWithoutGt).

parse_command_and_messages(" " ++ T) ->
	parse_command_and_messages(T);

parse_command_and_messages("/" ++ T) ->
	{Command, Rest} = parse_the_command(T),
	{Destinatary, Message} = parse_destinatary_and_message(Rest),
	{Command, Destinatary, Message};

parse_command_and_messages(Message) ->
	{message, Message}.

parse_the_command("to " ++ Str) ->
	{to, Str}.
	
parse_destinatary_and_message(Str) ->
	separate_first_word_of(Str).

separate_first_word_of(Str) ->
	SpaceIndex = string:chr(Str, $\s),
	{string:substr(Str, 1, SpaceIndex-1), string:substr(Str, SpaceIndex+1)}.

skip_to_gt(">" ++ T) -> T;
skip_to_gt([_|T])    -> skip_to_gt(T);
skip_to_gt([])       -> exit("no >").


parse_group(Str) ->
	Skipped = skip_to_gt(Str),
	trim(Skipped).
	
trim(" " ++ Str) -> Str.