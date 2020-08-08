.PHONY: all
all:
	stack build
	stack exec VB6-optim-exe

.PHONY: test
test:
	stack test

.PHONY: dependencies
dependencies:
	$(info Installing build dependencies with Stack:)
	stack install alex happy
