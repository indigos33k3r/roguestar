dontuse:
	echo "This makefile is for the convenience of the developer(s)."
	echo "Use:"
	echo "cabal configure"
	echo "cabal build"
	echo "cabal install"

configure:
	cabal-dev configure --ghc-options="-Werror" 

configure-profiling:
	--enable-library-profiling --enable-executable-profiling

build:
	cabal-dev build -j

clean:
	cabal-dev clean

run:
	./dist/build/roguestar-server/roguestar-server +RTS -xc -p -s 2> ./log/stdout.log

check: clean
	cabal-dev configure --ghc-options="-O0" --disable-library-profiling --disable-executable-profiling
	cabal-dev build

depends:
	cabal-dev install-deps -j
