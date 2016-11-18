.PRECIOUS: *.dat paper/dist/build/paper/paper

default: Prettiest.pdf

clean:
	rm -f *.aux *.ptb *.boxes *.log

paper/dist/build/paper/paper: paper/PM.hs
	cd paper
	cabal build

benchmark-40.dat: paper/dist/build/paper/paper
	paper/dist/build/paper/paper benchmark

Prettiest.pdf: paper/dist/build/paper/paper
	paper/dist/build/paper/paper
	pdflatex Prettiest
	bibtex Prettiest
	paper/dist/build/paper/paper
	pdflatex Prettiest

Prettiest.stack.pdf: benchmark-40.dat benchmark-80.dat paper/PM.hs
	stack build
	stack exec -- paper
	pdflatex Prettiest
	bibtex Prettiest
	stack exec -- paper
	pdflatex Prettiest

blog: blog.html

# %.html: %.org
# pandoc --email-obfuscation=references --smart --standalone --css=home.css --from=org --to=html --output=$@ $<

%.html: %.md
	pandoc --email-obfuscation=references --smart --standalone --css=home.css --from=markdown --to=html --output=$@ $<

%.md: %.org
	pandoc --from=org --to=markdown --output=$@ $<
