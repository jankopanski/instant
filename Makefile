all:
	$(MAKE) -C src all
	mv src/insc_jvm ./
	#mv src/insc_llvm ./

clean:
	$(MAKE) -C src clean
	rm -f insc_jvm insc_llvm
