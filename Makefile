gen-all: src/GenAll.hs
	mkdir -p obj && ghc --make src/GenAll -isrc -hidir obj -odir obj -o gen-all
