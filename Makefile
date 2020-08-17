
PROJECT 	= ejson

DEPS 		= jsx
TEST_DEPS	= proper

dep_jsx_commit 	= v3.0.0

COMPILE_FIRST		= ejson_trans
TEST_COMPILE_FIRST	= ejson_trans_test ejson_ext1 ejson_ext2 ejson_ext3
TEST_ERLC_OPTS 		= -pz $(CURDIR)/test +debug_info
EUNIT_OPTS			= verbose

all:: deps app
	$(gen_verbose) echo "Building ejson" 

# For Travis CI
test: deps app tests
	$(verbose) echo -n

.PHONY: ejson-transform
ejson-transform:
	$(verbose) echo "Compiling parse transform"
	$(gen_verbose) erlc -v $(TEST_ERLC_OPTS) -I include/ -o $(TEST_DIR) \
		$(foreach f,$(TEST_COMPILE_FIRST), test/$(f).erl) -pa ebin/


# Compile included test files first during testing 
test-dir: ejson-transform
	$(gen_verbose) erlc -v $(TEST_ERLC_OPTS) -I include/ -o $(TEST_DIR) \
		$(call core_find,$(TEST_DIR)/,*.erl) -pa ebin/

include erlang.mk

