all:
	stack ghc -- -odir tmp -hidir tmp -o a.out Main.hs
