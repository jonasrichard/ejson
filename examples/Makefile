
.PHONY: compile run

compile:
	../rebar -C rebar.config compile

run:
	erl -pz ../ebin ../deps/jsx/ebin apps/records/ebin -s records -s init stop
