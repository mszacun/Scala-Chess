package src;

class Queen(position : Int, color : Int, id : Int)
	extends Piece(position, color, id)
{
	val possibleDirections = Array(9, 11, -9, -11, 1, -1, 10, -10)

	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	def generateDirectionMoves(b : Board, acc : List[Move], dir : Int) :List[Move] = 
	{
		var tmpPos = position
		var result = acc
		while (true)
		{
			tmpPos += dir
			if (b.isEmpty(tmpPos))
				result = new QuietMove(position, tmpPos, 0, 0, 
				b.castlingRights) :: result
			else
			{
				if (b.isOccupiedByOpponent(tmpPos, color))
					result = new CaptureMove(position, tmpPos,
						b.castlingRights) :: result
				return result
			}
		}
		// never reaches here
		Nil
	}

	override def generateMoves(b : Board) = 
	{
		var result : List[Move] = Nil
		possibleDirections.foreach((dir : Int) => 
			result = generateDirectionMoves(b, result, dir))

		result
	}

	/* TODO: Implement */
	override def rank : Int = 0
}
