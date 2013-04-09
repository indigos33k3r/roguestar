dontuse:
	echo "This makefile is for the convenience of the developer(s)."
	echo "Use:"
	echo "cabal configure"
	echo "cabal build"
	echo "cabal install"

configure:
	cabal-dev configure --ghc-options="-Werror" --enable-library-profiling --enable-executable-profiling

build:
	cabal-dev build

clean:
	cabal-dev clean

run:
	./dist/build/roguestar-server/roguestar-server +RTS -xc -p -s 2> ./log/stdout.log
