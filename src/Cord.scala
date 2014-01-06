package src;

object Cord
{
	// translates array index into human-readable field name
	final def toString(arrayIndex : Int) = 
	{
		val row = getRow(arrayIndex)
		val column = getColumn(arrayIndex)

		(column + 'A'.toInt).toChar.toString + (row + 1).toString
	}

	 // Translates position on board into array index used in board representation
	final def fromString(position : String) = 
	{
		val columnChar = Character.toUpperCase(position.charAt(0))
		val column = columnChar - 'A'.toInt
		val row = position.charAt(1).toInt - '1'.toInt

		21 + row * 10 + column
	}

	// Extracts row number from position in array index form
	final def getRow(position : Int) = (position - 21) / 10

	// Extracts column number from position in array index form
	final def getColumn(position : Int) = (position - 21) % 10

	// converts index from 120 board repesentation to 64 board representation
	final val from120to64 = Array(
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
		64,  0,  1,  2,  3,  4,  5,  6,  7, 64,
	    64,  8,  9, 10, 11, 12, 13, 14, 15, 64,
   		64, 16, 17, 18, 19, 20, 21, 22, 23, 64,
    	64, 24, 25, 26, 27, 28, 29, 30, 31, 64,
    	64, 32, 33, 34, 35, 36, 37, 38, 39, 64,
    	64, 40, 41, 42, 43, 44, 45, 46, 47, 64,
    	64, 48, 49, 50, 51, 52, 53, 54, 55, 64,
    	64, 56, 57, 58, 59, 60, 61, 62, 63, 64,
    	64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    	64, 64, 64, 64, 64, 64, 64, 64, 64, 64
	)

	final val from64to120 = Array(
		21, 22, 23, 24, 25, 26, 27, 28,
		31, 32, 33, 34, 35, 36, 37, 38,
		41, 42, 43, 44, 45, 46, 47, 48,
		51, 52, 53, 54, 55, 56, 57, 58,
		61, 62, 63, 64, 65, 66, 67, 68,
		71, 72, 73, 74, 75, 76, 77, 78,
		81, 82, 83, 84, 85, 86, 87, 88,
		91, 92, 93, 94, 95, 96, 97, 98
	)

	// Auxiliary functions to move index array in specifed direction
	// N -> north, S -> south, E -> east, W -> west
	final def moveN(position : Int, count : Int) = position - 10 * count

	final def moveS(position : Int, count : Int) = position + 10 * count

	final def moveE(position : Int, count : Int) = position + count

	final def moveW(position : Int, count : Int) = position - count

	final def moveNE(position : Int, count : Int) = position - 10 * count + count

	final def moveNW(position : Int, count : Int) = position - 10 * count - count

	final def moveSE(position : Int, count : Int) = position + 10 * count + count

	final def moveSW(position : Int, count : Int) = position + 10 * count - count
}
