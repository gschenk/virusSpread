TARGET=virus

FLAGS := -O3 -dynamic

SVGS = plotR.svg plot30.svg plot15.svg

DATSCRIPT = getDats.sh
DATS = virus30.dat

.PHONY: all binary plots data clean



all: binary plots data

data: $(DATS)

$(DATS): $(DATSCRIPT) $(TARGET) 
	sh $<

plots: $(SVGS)

%.svg : %.gnu $(DATS)
	gnuplot -e "set terminal svg" $< > $@

binary: $(TARGET)

% : %.hs
	ghc $(FLAGS) -o $@ $<

clean:
	rm -f *.o *.hi
