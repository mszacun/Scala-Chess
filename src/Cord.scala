package src;

object Cord
{
	// translates array index into human-readable field name
	def toString(arrayIndex : Int) = 
	{
		val row = (arrayIndex - 21) / 10
		val column = (arrayIndex - 21) % 10

		(column + 'A'.toInt).toChar.toString + (row + 1).toString
	}

	 // Translates position on board into array index used in board representation
	def fromString(position : String) = 
	{
		val column = position.charAt(0).toInt - 'A'.toInt
		val row = position.charAt(1).toInt - '1'.toInt

		21 + row * 10 + column
	}
}
