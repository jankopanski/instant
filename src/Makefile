CC = ghc
PARSER_SRC = AbsInstant.hs ErrM.hs LexInstant.hs ParInstant.hs PrintInstant.hs SkelInstant.hs

all: jvm llvm

jvm: $(PARSER_SRC) CompilerJVM.hs cabal
	$(CC) -o insc_jvm CompilerJVM.hs

llvm: $(PARSER_SRC) CompilerLLVM.hs cabal
	$(CC) -o insc_llvm CompilerLLVM.hs

cabal:
	cabal install mtl

clean:
	rm -f *.hi *.o insc_jvm insc_llvm
