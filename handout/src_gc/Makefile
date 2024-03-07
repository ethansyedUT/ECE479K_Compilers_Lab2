LEXER = ../reference-binaries/lexer
PARSER = ../reference-binaries/parser
SEMANT = ../reference-binaries/semant
CLANG = clang-15
OPT = opt

GC_LIB = ./gc.so
GC_FLAGS = -Wl,-rpath $(realpath .)

debug = true
lab2 = true
proj_dir = ../src

ifeq ($(debug),true)
  CGENOPTS = -d
else
  CGENOPTS =
endif

ifeq ($(lab2),true)
  CGEN = cgen-2
  COOLRT = ../reference-binaries/coolrt.o
else
  CGEN = cgen-1
  COOLRT =
endif

SRCS := $(wildcard *.cl)

# Disable built-in rules and variables
.SUFFIXES:

.PRECIOUS: %.ast %.ll %-o3.ll %.bin

default: all
all: $(SRCS:%.cl=%.out)
verify: $(SRCS:%.cl=%.verify)
check: $(SRCS:%.cl=%.check)

cgen-1:
	make -j -C $(proj_dir) cgen-1

cgen-2:
	make -j -C $(proj_dir) cgen-2

%.ast: %.cl
	$(LEXER) $< | $(PARSER) | $(SEMANT) > $@

%.ll: %.ast $(CGEN)
	$(proj_dir)/$(CGEN) $(CGENOPTS) < $< > $@

%-o3.ll: %.ll
	$(OPT) -O3 -S $< -f -o $*-o3.ll

%.bin: %-o3.ll $(COOLRT) $(GC_LIB)
	$(CLANG) -g $+ $(GC_FLAGS) -o $@

%.verify: %.ll
	$(OPT) -S -verify $<

%.out: %.bin
	./$< > $@ || true

%.check: %.out
	diff -u $< $(<:%.out=%.refout)

clean:
	-rm -f *.bin *.ll *.out *.ast *.verify
