all:
	$(MAKE) -C src all
	cp src/insc_jvm ./
	cp src/insc_llvm ./

clean:
	$(MAKE) -C src clean
	rm -f insc_jvm insc_llvm
