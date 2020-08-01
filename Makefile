.PHONY: all
all:
	stack build
	stack exec VB6-optim-exe

.PHONY: test
test:
	stack test
