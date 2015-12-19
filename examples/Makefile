
.PHONY: try
.DEFAULT_GOAL: try

PROJECT = records

PZ = ../deps/jsx/ebin ../deps/ebin ebin

try: app
	erl -pz $(PZ) -s records -s init stop

include ../erlang.mk

