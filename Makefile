
PROJECT 	= ejson

DEPS 		= jsx
TEST_DEPS	= proper

dep_jsx_commit 	= 2.7.2

COMPILE_FIRST		= ejson_trans
TEST_COMPILE_FIRST	= ejson_trans_test
TEST_ERLC_OPTS 		= -pz $(CURDIR)/test +debug_info

all:: deps app
	$(gen_verbose) echo "Building ejson" 

# For Travis CI
test: deps app tests
	$(verbose) echo -n

.PHONY: ejson-transform
ejson-transform:
	$(gen_verbose) erlc -v $(TEST_ERLC_OPTS) -I include/ -o $(TEST_DIR) \
		$(foreach f,$(TEST_COMPILE_FIRST), test/$(f).erl) -pa ebin/

# This hack is needed since tests call clean-test-dir
test-dir: ejson-transform
	$(verbose) echo "Compiling parse transform"
	$(gen_verbose) erlc -v $(TEST_ERLC_OPTS) -I include/ -o $(TEST_DIR) \
		$(call core_find,$(TEST_DIR)/,*.erl) -pa ebin/

include erlang.mk

