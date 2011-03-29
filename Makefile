RANDY_SRC=Main.hs Cube.hs Graphics.hs Points.hs Bindings.hs Circle.hs SF.hs

.PHONY: all

all: glApp

glApp: $(RANDY_SRC)
	ghc --make -o $@ $(RANDY_SRC)

clean:
	rm -f *.hi *.o glApp
