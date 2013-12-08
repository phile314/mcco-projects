CABAL-CONFIGURE-FLAGS := --user --enable-tests
CABAL-BUILD-FLAGS     :=

all : haskell

src/CCO/Diag/AG.hs : src/CCO/Diag/AG.ag src/CCO/Diag/AG/Base.ag
	uuagc -Hdcfws -P src/CCO/Diag src/CCO/Diag/AG.ag

src/CCO/Picture/AG.hs : src/CCO/Picture/AG.ag src/CCO/Picture/AG/Base.ag \
		src/CCO/Picture/AG/Printing.ag
	uuagc -Hdcfws -P src/CCO/Picture src/CCO/Picture/AG.ag

src/Type/AG.hs : src/Type/Ag.ag
	uuagc -H -P src/Type src/CCO/Type/AG.ag

haskell : src/CCO/Diag/AG.hs src/CCO/Picture/AG.hs
	runhaskell Setup.hs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.hs build $(CABAL-BUILD-FLAGS)

test : haskell
	runhaskell Setup.hs test

.PHONY : haskell
