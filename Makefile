.PHONY : phony

test : phony
	cabal run test

bench-nonempty : phony
	cabal run bench -- -o bench-nonempty.html NonEmpty

bench-tree : phony
	cabal run bench -- -o bench-tree.html Tree
