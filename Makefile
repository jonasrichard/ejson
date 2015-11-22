
PROJECT 	= ejson

DEPS 		= jsx
TEST_DEPS	= proper

dep_jsx_commit 	= 2.7.2

COMPILE_FIRST		= ejson_trans
TEST_COMPILE_FIRST	= ejson_trans_test
TEST_ERLC_OPTS 		= -pz $(CURDIR)/test

tests:: app
	$(gen_verbose) erlc -v $(TEST_ERLC_OPTS) -I include/ -o $(TEST_DIR) \
		$(foreach f,$(TEST_COMPILE_FIRST), test/$(f).erl) -pa ebin/

include erlang.mk

