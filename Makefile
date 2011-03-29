RANDY_SRC:=$(wildcard *.hs)

.PHONY: all

all: glApp

glApp: $(RANDY_SRC)
	ghc --make -o $@ $(RANDY_SRC)

clean:
	rm -f *.hi *.o glApp
