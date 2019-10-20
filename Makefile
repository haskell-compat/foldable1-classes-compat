.PHONY : phony

test : phony
	cabal run test

doctest : phony
	doctest --fast src/

bench-nonempty : phony
	cabal run bench -- -o bench-nonempty.html NonEmpty

bench-tree : phony
	cabal run bench -- -o bench-tree.html Tree

haddock :
	cabal haddock --haddock-hyperlink-source

proposal2.html : proposal2.md
	pandoc --metadata "pagetitle=Foldable1 proposal" -s proposal2.md -o proposal2.html
