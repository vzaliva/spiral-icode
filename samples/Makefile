SPIRAL_HOME=~/src/spiral

all: dft16.o dft16.ll dft16.s dwmonitor.o dwmonitor.ll dwmonitor.s

SPIRAL_C_FLAGS=-O3 -g0 -c -m64 -msse4
SPIRAL_C_S_FLAGS=-S -Wextra -fverbose-asm
SPIRAL_C_LL_FLAGS=-c -S -O3 -g0 -msse4 -emit-llvm

clean:
	rm -f dft16.o dft16.s dft16.s.txt dwmonitor.s dwmonitor.o

.c.s: $@ Makefile
	gcc -I $(SPIRAL_HOME) $(SPIRAL_C_FLAGS) $(SPIRAL_C_S_FLAGS) $<

.c.o: $@ Makefile
	 gcc -I $(SPIRAL_HOME) $(SPIRAL_C_FLAGS) $<

.c.ll: $@ Makefile
	clang -I $(SPIRAL_HOME) $(SPIRAL_C_LL_FLAGS) $<

#dft16.s.txt: dft16.c Makefile
#	rm -f dft16.s.txt
#	gcc -I $(SPIRAL_HOME) -Wa,-adhln -g -c -O0 -m64 dft16.c > dft16.s.txt






