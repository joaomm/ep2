%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(chat_server).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [delete/2, foreach/2, map/2, member/2,reverse/2]).

-compile(export_all).


start() ->
    start_server(),
    lib_chan:start_server("chat.conf").

start_server() ->
    register(chat_server, 
	     spawn(fun() ->
			   process_flag(trap_exit, true),
			   Val= (catch server_loop([])),
			   io:format("Server terminated with:~p~n",[Val])
		   end)).

server_loop(Groups) ->
    receive
	{mm, Channel, {login, Group, Nickname, Node}} ->
	    login(Channel, Groups, Group, Nickname, Node);
	{mm_closed, _} ->
	    server_loop(Groups); 
	{'EXIT', GroupPid, allGone} ->
	    UpdatedGroups = remove_group(GroupPid, Groups),
	    server_loop(UpdatedGroups);
	Msg ->
	    io:format("Server received Msg=~p~n", [Msg]),
	    server_loop(Groups)
    end.

login(Channel, Groups, GroupName, Nickname, Node) -> 
	case lookup(GroupName, Groups) of
		{ok, GroupPid} ->
	    	log_nickname_to_group(Channel, GroupPid, Nickname),
	    	server_loop(Groups);
		group_not_found ->
	    	NewGroupPid = create_new_group_and_add_nickname(Channel, GroupName, Nickname, Node),
			GroupsIncludingNewGroup = add_group(GroupName, NewGroupPid, Groups),
	    	server_loop(GroupsIncludingNewGroup)
    end.

log_nickname_to_group(Channel, GroupPid, Nickname) ->
	GroupPid ! {login, Channel, Nickname}.

create_new_group_and_add_nickname(Channel, GroupName, Nickname, Node) ->
	spawn_link(Node, fun() -> chat_group:start(Channel, GroupName, Nickname)  end).

add_group(NewGroup, NewGroupPid, Groups) ->
	[{NewGroup, NewGroupPid} | Groups].

lookup(G, [{G,Pid}|_]) -> {ok, Pid};
lookup(G, [_|T])       -> lookup(G, T);
lookup(_,[])           -> group_not_found.

remove_group(Pid, [{G,Pid}|T]) -> io:format("~p removed~n",[G]), T;
remove_group(Pid, [H|T])       -> [H|remove_group(Pid, T)];
remove_group(_, [])            -> [].

