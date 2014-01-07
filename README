Scala-Chess is chess engine and game written in scala:

Implemented features:
	* 10x12 board representation and piece list
	* Complete move generation functions, tested according to perft tests
		(http://chessprogramming.wikispaces.com/Perft+Results)
	* Alfabeta(in minimax fashion) search function for AI thinking with:
		- move ordering (killers, pv moves, captures according to LVA/MVV rule)
		- quiesence search
		- in-check extension
		- aspiration window search
		- transposition table
		- iterative deepening
		- really simple evaluation function (takes into consideration
			only material value and position of pieces)
	* GUI for playing (TODO)

	Engine is able to think about 7-moves deep in about 5 seconds, i know that's not
a lot comparing to other engines, but keep in mind, that this engine is written in
scala, running on JVM platform, so it probably will never as quick as engines 
written in C.
	The slowest part of engine is move generation, and i think the biggest optimization
should be done here. My engine uses 10x12 board representation, which is not the
slowest one, but still is a performance issue comparing to using bitboards.
	I wanted to use as much functional and object-oriented style as possible 
without hurting performance, this is another reason, why may be slow
	I think my engine is the best for advanced chess players, because it is
not one of those unbeatable enginwa, that always wins, altough it doesn't
make simple mistakes. In my opinion advanced players will enjoy ability to
sometimes win with computer
