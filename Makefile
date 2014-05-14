dontuse:
	@echo "This makefile is for the convenience of the developer(s)."
	@echo "Use:"
	@echo "cabal configure"
	@echo "cabal build"
	@echo "cabal install"

setup:
	cabal sandbox init
	cabal install --only-dependencies -j
	cabal configure --ghc-options="-Werror"
	cabal build

takedown:
	cabal clean
	cabal sandbox delete
