
PROJECT 	= ejson

DEPS 		= jsx
TEST_DEPS	= proper

dep_jsx_commit 	= 2.7.2

COMPILE_FIRST		= ejson_trans
TEST_COMPILE_FIRST	= ejson_trans_test
TEST_ERLC_OPTS 		+= -pz $(CURDIR)/test

include erlang.mk

