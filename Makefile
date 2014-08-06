
ERL_APPS = erts \
		   kernel \
		   stdlib

.PHONY: analyze compile example test

compile:
	./rebar compile

test:
	./rebar eunit apps=ejson

.dialyzer_plt:
	dialyzer --output_plt $@ --build_plt --apps $(ERL_APPS) deps/jsx/ebin

analyze: .dialyzer_plt
	dialyzer --no_check_plt --no_native -Wrace_conditions --plt $< --apps ebin
