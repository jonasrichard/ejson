
ERL_APPS = erts \
		   kernel \
		   stdlib

.PHONY: analyze compile clean example test

clean:
	./rebar clean

compile:
	./rebar compile

test:
	./rebar eunit apps=ejson

.dialyzer_plt:
	dialyzer --output_plt $@ --build_plt --apps $(ERL_APPS) deps/jsx/ebin

analyze: .dialyzer_plt
	dialyzer --no_check_plt --no_native -Wrace_conditions --plt $< --apps ebin

example:
	cd examples; \
	../rebar -C rebar.config compile;
	erl -pz deps/jsx/ebin ebin examples/apps/records/ebin -noshell -s records -s init stop

