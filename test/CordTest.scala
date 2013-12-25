package test;

import src.Cord;

class CordTest extends Test("CordTest")
{
	def doAllTests = 
	{
		testFromStringConversion
		testToStringConversion
	}

	def testFromStringConversion = 
	{
		assert(Cord.fromString("A1") == 21)
		assert(Cord.fromString("D4") == 54)

		// small letters
		assert(Cord.fromString("c6") == 73)
		assert(Cord.fromString("e3") == 45)
	}

	def testToStringConversion = 
	{
		assert(Cord.toString(35) == "E2")
		assert(Cord.toString(42) == "B3")
	}
}
