SCALAC = scalac
FLAGS = -optimise 
SCALA = scala

default: source

all: source tests

source: 
	$(SCALAC) $(FLAGS) src/*.scala
tests:
	$(SCALAC) $(FLAGS) test/*.scala

	$(SCALA) test.AllTests

clean:
	rm src/*.class
	rm test/*.class
