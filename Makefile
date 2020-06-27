EXEC=jc
FLAGS=

all: jc

clean: 
	rm -rf *.o *.hi VM/*.o VM/*.hi Jack/*.o Jack/*.hi

mrproper: clean
	rm -rf $(EXEC)

jc:
	ghc -o $@ Main.hs
