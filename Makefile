all: compile

get-deps:
	@./rebar get-deps

compile: get-deps
	@./rebar compile

dev:
	./init.sh start-dev

clean:
	./rebar clean