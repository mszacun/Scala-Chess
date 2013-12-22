package src;

object Cord
{
	// translates array index into human-readable field name
	def toString(arrayIndex : Int) = 
	{
		val row = getRow(arrayIndex)
		val column = getColumn(arrayIndex)

		(column + 'A'.toInt).toChar.toString + (row + 1).toString
	}

	 // Translates position on board into array index used in board representation
	def fromString(position : String) = 
	{
		val column = position.charAt(0).toInt - 'A'.toInt
		val row = position.charAt(1).toInt - '1'.toInt

		21 + row * 10 + column
	}

	// Extracts row number from position in array index form
	def getRow(position : Int) = (position - 21) / 10

	// Extracts column number from position in array index form
	def getColumn(position : Int) = (position - 21) % 10

	// Auxiliary functions to move index array in specifed direction
	// N -> north, S -> south, E -> east, W -> west
	def moveN(position : Int, count : Int) = position - 10 * count

	def moveS(position : Int, count : Int) = position + 10 * count

	def moveE(position : Int, count : Int) = position + count

	def moveW(position : Int, count : Int) = position - count

	def moveNE(position : Int, count : Int) = position - 10 * count + count

	def moveNW(position : Int, count : Int) = position - 10 * count - count

	def moveSE(position : Int, count : Int) = position + 10 * count + count

	def moveSW(position : Int, count : Int) = position + 10 * count - count
}
