all:
	stack ghc -- -Wall -fno-warn-unused-do-bind -odir tmp -hidir tmp -o a.out Main.hs
