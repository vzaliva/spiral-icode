SPIRAL_HOME=~/src/spiral

all: dft16.o dft16.ll dft16.s dft16.s.txt dwmonitor.o

clean:
	rm -f dft16.o dft16.s dft16.s.txt dwmonitor.s dwmonitor.o

dft16.s: dft16.c Makefile
	gcc -I $(SPIRAL_HOME) -c -S -O0 -m64 -Wextra -fverbose-asm dft16.c 

dft16.s.txt: dft16.c Makefile
	rm -f dft16.s.txt
	gcc -I $(SPIRAL_HOME) -Wa,-adhln -g -c -O0 -m64 dft16.c > dft16.s.txt

dft16.ll: dft16.c Makefile
	clang -I $(SPIRAL_HOME) -c -S -O0 -g0 -msse4 -emit-llvm dft16.c

dft16.o: dft16.c Makefile
	 clang -I $(SPIRAL_HOME) -c -m64 -msse4 dft16.c 

dwmonitor.o: dwmonitor.c Makefile
	 gcc -I $(SPIRAL_HOME) -c -m64 -msse4 dwmonitor.c 



