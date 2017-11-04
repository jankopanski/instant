PARSER_SRC = AbsInstant.hs ErrM.hs LexInstant.hs ParInstant.hs PrintInstant.hs SkelInstant.hs

all: jvm

jvm: $(PARSER_SRC) CompilerJVM.hs
	ghc -o insc_jvm CompilerJVM.hs

llvm: $(PARSER_SRC) CompilerLLVM.hs
	ghc -o insc_llvm CompilerLLVM.hs

clean:
	rm -f *.hi *.o insc_jvm insc_llvm
