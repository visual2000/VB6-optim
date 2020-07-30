.PHONY: all
all:
	stack build
	cat examples/Module1.bas | stack exec VB6-optim-exe
