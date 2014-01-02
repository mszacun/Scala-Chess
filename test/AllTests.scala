package test;


object AllTests extends Test("All")
{
	val tests = new MoveGenerationTest :: Nil

	def doAllTests = tests.foreach((t : Test) => {
		t.doAllTests
		println("Done: " + t.name)})

	def main(args : Array[String]) = doAllTests
}

