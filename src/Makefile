
PARSER_SOURCES = AbsInstant.hs ErrM.hs LexInstant.hs ParInstant.hs PrintInstant.hs SkelInstant.hs

all: jvm

jvm: $(PARSER_SOURCES) CompilerJVM.hs
	ghc -o insc_jvm CompilerJVM.hs

clean:
	rm -f *.hi *.o insc_jvm insc_llvm
