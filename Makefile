all: test

fmt:
	dune build @fmt --auto-promote

build: fmt
	dune build

test:
	dune runtest --auto-promote

watch:
	dune build @fmt @runtest --auto-promote --watch

.PHONY: all build test fmt watch
