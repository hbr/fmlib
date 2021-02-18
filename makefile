.PHONY: all
all: doc test



.PHONY: doc
doc:
	dune build @doc



.PHONY: test
test:
	dune build @runtest
