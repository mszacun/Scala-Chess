SCALAC = scalac
FLAGS = -g
SCALA = scala

default: source

all: source tests

source: 
	$(SCALAC) src/Cord.scala

tests:
	$(SCALAC) test/Test.scala 
	$(SCALAC) test/CordTest.scala
	$(SCALAC) test/AllTests.scala

	$(SCALA) test.AllTests

clean:
	rm src/*.class
	rm test/*.class
