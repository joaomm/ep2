.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS = chat_client chat_group chat_server \
	io_widget \
	lib_chan_mm lib_chan lib_chan_cs lib_chan_auth \
	lib_md5 \
	mod_chat_controller


ERL = erl -boot start_clean 

compile: ${MODS:%=%.beam}

chat_client: compile
	erl -s chat_client start -sname bilbo
	
chat_server: compile
	erl -s chat_server start -sname gandalf

clean:	
	rm -rf *.beam erl_crash.dump
