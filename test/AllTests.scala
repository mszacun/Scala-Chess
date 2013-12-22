package test;


object AllTests extends Test
{
	val tests = new CordTest() :: new SimplePiecesMovesTest() :: Nil

	def doAllTests = tests.foreach((t : Test) => t.doAllTests)

	def main(args : Array[String]) = doAllTests
}

