SCALAC = scalac
FLAGS = -g
SCALA = scala

default: source

all: source tests

source: 
	$(SCALAC) src/*.scala
tests:
	$(SCALAC) test/*.scala

	$(SCALA) test.AllTests

clean:
	rm src/*.class
	rm test/*.class
