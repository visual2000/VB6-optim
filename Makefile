.PHONY: all
all:
	stack build
	cat examples/HitFuncs.bas | stack exec VB6-optim-exe
