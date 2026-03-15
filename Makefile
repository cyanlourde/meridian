.PHONY: build test clean

build:
	dune build

test:
	dune runtest

clean:
	dune clean
