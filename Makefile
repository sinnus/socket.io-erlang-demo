all:
	rebar compile skip_deps=true

deps:
	rebar get-deps

compile: deps
	rebar compile

test:
	rebar eunit skip_deps=true
