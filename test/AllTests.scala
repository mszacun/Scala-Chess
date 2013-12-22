package test;


object AllTests extends Test
{
	val tests = new CordTest() :: Nil

	def doAllTests = tests.foreach((t : Test) => t.doAllTests)

	def main(args : Array[String]) = doAllTests
}

