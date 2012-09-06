
all:
	ghc Calc
	ghc PrimeTester

less_dirty:
	rm -rf *.hi *.o

clean: less_dirty
	rm -rf Calc PrimeTester

.PHONY : less_dirty
.PHONY : clean

