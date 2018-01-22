SPIRAL_HOME=~/src/spiral

all: dft16.o dft16.ll dft16.s dwmonitor.o dwmonitor.ll dwmonitor.s dft16test.ll dft16test dft16test-cl

SPIRAL_C_FLAGS=-O3 -g0 -m64 -msse4
SPIRAL_C_S_FLAGS=-S -Wextra -fverbose-asm
SPIRAL_C_LL_FLAGS=-c -S -O3 -g0 -msse4 -emit-llvm

.PHONY: clean

clean:
	rm -f dft16.o dft16.ll dft16.s dwmonitor.o dwmonitor.ll dwmonitor.s dft16test.ll dft16test dft16test-cl

dft16test: Makefile dft16test.o dft16.c
	 gcc -I $(SPIRAL_HOME) $(SPIRAL_C_FLAGS) -o $@ dft16test.o

dft16test-cl: Makefile dft16.c
	 clang -I $(SPIRAL_HOME) $(SPIRAL_C_FLAGS) -o $@ dft16test.c

%.s: %.c Makefile
	gcc -c -I $(SPIRAL_HOME) $(SPIRAL_C_FLAGS) $(SPIRAL_C_S_FLAGS) $<

%.o: %.c Makefile
	 gcc -c -I $(SPIRAL_HOME) $(SPIRAL_C_FLAGS) $<

%.ll: %.c Makefile
	clang -I $(SPIRAL_HOME) $(SPIRAL_C_LL_FLAGS) $<

#dft16.s.txt: dft16.c Makefile
#	rm -f dft16.s.txt
#	gcc -I $(SPIRAL_HOME) -Wa,-adhln -g -c -O0 -m64 dft16.c > dft16.s.txt






