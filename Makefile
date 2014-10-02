APPLICATION := bstr

ERL := erl
EPATH := -pa ebin -pz deps/mochiweb/ebin
TEST_EPATH := -pa .eunit -pz deps/mochiweb/ebin

.PHONY: all doc clean test

all: compile

travis_ci: \
	bootstrap \
	compile \
	test

bootstrap:
	@./rebar get-deps

compile:
	@./rebar compile

doc:
	@./rebar skip_deps=true doc

clean:
	@./rebar skip_deps=true clean

depclean:
	@./rebar clean

distclean:
	@./rebar delete-deps

dialyze: compile
	@dialyzer -r .

test:
	@./rebar skip_deps=true eunit

console: compile
	$(ERL) -sname $(APPLICATION) $(EPATH)

test-console: compile
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH)

