APPLICATION := bstr

ERL := erl
EPATH := -pa ebin -pz deps/mochiweb/ebin
TEST_EPATH := -pa .eunit -pz deps/mochiweb/ebin

.PHONY: all doc clean test

all: compile

bootstrap:
	@./rebar get-deps

compile:
	@./rebar compile

doc:
	@./rebar doc

clean:
	@./rebar clean

distclean:
	@./rebar delete-deps

build-plt: compile
	@./rebar build-plt

check-plt: compile
	@./rebar check-plt

dialyze: compile
	@./rebar dialyze

test:
	@./rebar eunit

console: compile
	$(ERL) -sname $(APPLICATION) $(EPATH)

test-console: compile
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH)

