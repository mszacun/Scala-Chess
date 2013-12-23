package src;

class King(position : Int, color : Int, id : Int)
	extends Piece(position, color, id)
{
	val possibleDirections = Array(9, 11, -9, -11, 1, -1, 10, -10)

	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	override def generateMoves(b : Board) = 
	{
		var result : List[Move] = Nil
		val castlingRightsAfter = Array(false, false, false, false, false)
		possibleDirections.foreach((dir : Int) => 
		{
			val tmpPos = position + dir
			if (b.isEmpty(tmpPos))
				result = new QuietMove(position, tmpPos, 0, 0,
					castlingRightsAfter) :: result
			else
				if (b.isOccupiedByOpponent(tmpPos, color))
					result = new CaptureMove(position, tmpPos,
						castlingRightsAfter) :: result
		})

		result
	}

	/* TODO: Implement */
	override def rank : Int = 0
}
