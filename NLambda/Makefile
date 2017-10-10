.PHONY: all clean

all: minimize fifo_minimize running_minimize

minimize: minimize.hs
	ghc minimize.hs

fifo_minimize: Fifo.hs fifo_minimize.hs
	ghc fifo_minimize.hs

running_minimize: RunningExample.hs running_minimize.hs
	ghc running_minimize.hs

clean:
	rm -f minimize minimize.o minimize.hi
	rm -f fifo_minimize fifo_minimize.o fifo_minimize.hi Fifo.o Fifo.hi
	rm -f running_minimize running_minimize.o running_minimize.hi RunningExample.o RunningExample.hi
