%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(chat_group).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [foreach/2, map/2, reverse/2]).

-export([start/2]).

start(C, Nick) ->
    process_flag(trap_exit, true),
    controller(C, self()),
    send(C, ack),
    self() ! {chan, C, {relay, Nick, "I'm starting the group"}},
    group_controller([{C,Nick}]).



delete(Pid, [{Pid,Nick}|T], L) -> {Nick, reverse(T, L)};
delete(Pid, [H|T], L)          -> delete(Pid, T, [H|L]);
delete(_, [], L)               -> {"????", L}.



group_controller([]) ->
    exit(allGone);
group_controller(NicknamesList) ->
    receive
	{chan, Channel, {relay, Nick, Str}} ->
	    foreach(fun({Pid,_}) -> send(Pid, {msg,Nick,Channel,Str}) end, NicknamesList),
	    group_controller(NicknamesList);
	{login, Channel, Nick} ->
	    controller(Channel, self()),
	    send(Channel, ack),
	    self() ! {chan, Channel, {relay, Nick, "I'm joining the group"}},
		NewNicknamesList = [{Channel,Nick}|NicknamesList],
		send_group_list_to_all(NewNicknamesList),
	    group_controller(NewNicknamesList);
	{chan_closed, Channel} ->
	    {Nick, NewNicknamesList} = delete(Channel, NicknamesList, []),
	    self() ! {chan, Channel, {relay, Nick, "I'm leaving the group"}},
		send_group_list_to_all(NewNicknamesList),
	    group_controller(NewNicknamesList);
	Any ->
	    io:format("group controller received Msg=~p~n", [Any]),
	    group_controller(NicknamesList)
    end.

send_group_list_to_all(NicknamesList) ->
	Nicknames = filter_nicknames(NicknamesList),
	foreach(fun({Pid,_}) -> send(Pid, {group_list, Nicknames}) end, NicknamesList).

filter_nicknames(NicknamesList) ->
	map(fun select_nickname/1, NicknamesList).

select_nickname({_Pid, Nick}) -> Nick.