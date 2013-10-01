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

check: clean
	cabal-dev configure --ghc-options="-O0" --disable-library-profiling --disable-executable-profiling
	cabal-dev build

depends:
	cabal-dev install cipher-aes-0.1.8
	cabal-dev install MaybeT
	cabal-dev install MonadCatchIO-transformers
	cabal-dev install aeson
	cabal-dev install data-lens-template
	cabal-dev install data-memocombinators
	cabal-dev install hastache
	cabal-dev install hslogger
	cabal-dev install mwc-random
	cabal-dev install snap-core
	cabal-dev install snap-server
	cabal-dev install snap
	cabal-dev install streams
	cabal-dev install system-uuid
	cabal-dev install data-lens-template
	cabal-dev install snap
	cabal-dev install streams
