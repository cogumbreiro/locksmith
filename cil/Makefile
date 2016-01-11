#This Makefile is a wrapper for Makefile.cil
setup: setup
%: 
	if ! test -f Makefile.cil ;then echo Run configure first ;fi
	$(MAKE) -f Makefile.cil $*
