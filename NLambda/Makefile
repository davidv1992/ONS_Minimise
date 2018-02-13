.PHONY: all clean

all: minimize fifo_minimize running_minimize Lmax_minimize Lint_minimize

minimize: minimize.hs
	ghc minimize.hs

fifo_minimize: Fifo.hs fifo_minimize.hs
	ghc fifo_minimize.hs

running_minimize: RunningExample.hs running_minimize.hs
	ghc running_minimize.hs

Lmax_minimize: Lmax_minimize.hs Lmax.hs
	ghc Lmax_minimize.hs

Lint_minimize: Lint_minimize.hs Lint.hs
	ghc Lint_minimize.hs

clean:
	rm -f minimize minimize.o minimize.hi
	rm -f fifo_minimize fifo_minimize.o fifo_minimize.hi Fifo.o Fifo.hi
	rm -f running_minimize running_minimize.o running_minimize.hi RunningExample.o RunningExample.hi
	rm -f Lmax_minimize Lmax_minimize.o Lmax_minimize.hi Lmax.o Lmax.hi
	rm -f Lint_minimize Lint_minimize.o Lint_minimize.hi Lint.o Lint.hi
