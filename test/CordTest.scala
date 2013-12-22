package test;

import src.Cord;

class CordTest extends Test
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
	}

	def testToStringConversion = 
	{
		assert(Cord.toString(35) == "E2")
		assert(Cord.toString(42) == "B3")
	}
}
