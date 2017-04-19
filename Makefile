.PRECIOUS: *.dat .styx/bin/paper

default: Prettiest.pdf

clean:
	rm -f *.aux *.ptb *.boxes *.log

.styx/bin/paper .styx/bin/bench: paper/PM.hs
	styx cabal install pretty-paper

benchmark-40.dat: .styx/bin/paper
	.styx/bin/paper benchmark

benchmark-random.dat: .styx/bin/bench
	.styx/bin/bench

Prettiest.pdf: .styx/bin/paper
	styx exec -- paper
	styx exec -- pdflatex Prettiest
	styx exec -- bibtex Prettiest
	styx exec -- paper
	styx exec -- pdflatex Prettiest

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
