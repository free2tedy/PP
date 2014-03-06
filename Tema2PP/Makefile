.PHONY: all compile run clean

TESTER = Tester

all: compile

compile:
	ghc --make -o $(TESTER) $(TESTER)

run: compile
	./$(TESTER) tests.txt

clean:
	rm *.o *.hi $(TESTER)