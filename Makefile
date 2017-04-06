gen-all: src/GenAll.hs
	mkdir -p obj && ghc --make src/GenAll -isrc -hidir obj -odir obj -o gen-all

install: 
	sed 's/"http:\/\/localhost:3000\/A\/" + wd + ".html#Spanish",/"https:\/\/en.wiktionary.org\/wiki\/" + wd + "#Spanish",/' web/index.html | sudo tee /var/www/html/charism/index.html >/dev/null
