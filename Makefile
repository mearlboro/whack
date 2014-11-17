# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler
# Locations

all:
	cabal update
	cabal install --only-dependencies
	cabal configure
	cabal build

clean:
	rm -f *.hi *.o
	rm -f Wacc/*.hi Wacc/*.o
