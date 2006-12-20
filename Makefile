HS_FLAGS = 	-hidir products/ \
		-odir products/ \
		-isrc/:products \
		-Wall \
		-fno-warn-type-defaults \
		--make src/Main.hs \
		-o products/roguestar-engine

default : ghc

release : ghc-release

update :
	darcs pull

install :
	install products/roguestar-engine /usr/local/bin/

clean :
	-rm -f products/*.o 2> /dev/null
	-rm -f products/*.hi 2> /dev/null
	-rm -f products/roguestar-engine 2> /dev/null
	${MAKE} -C haddock clean

doc :
	${MAKE} -C haddock

ghc :
	@echo "warning: you're building with development flags on (-Werror, no optimization)"
	@echo "         did you want to 'make release' ?"
	ghc 	-Werror ${HS_FLAGS}

ghc-prof :
	ghc	${HS_FLAGS} -prof -auto-all

ghc-release :
	ghc	-O2 -fvia-c ${HS_FLAGS}

check:
	${MAKE} clean
	${MAKE} ghc-release
	./products/roguestar-engine tests
	${MAKE} clean
	-darcs whatsnew -l

dist:
	darcs dist

headache:
	headache -c header/license-header.conf -h header/license-header src/*.hs

headache-remove:
	headache -c header/license-header.conf -h header/license-header -r src/*.hs

.PHONY : default clean doc ghc ghc-release check dist headache headache-remove release
