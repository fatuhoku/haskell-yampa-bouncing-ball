RANDY_SRC=Main.hs

.PHONY: all

all: randy

randy: $(RANDY_SRC)
	ghc --make -o $@ $(RANDY_SRC)

clean:
	rm -f *.hi *.o randy

