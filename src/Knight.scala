package src;

object Knight
{
	val possibleMovesDirection = Array(-19, -21, -12, -8, 19, 21, 8, 12)
}

class Knight(position : Int, color : Int, id : Int)
	extends Piece(position, color, id, Piece.KNIGHT)
{
	
	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	def generateMoves(b : Board) = 
	{
		var result : List[Move] = Nil
		Knight.possibleMovesDirection.foreach((dir : Int) => 
		{
			if (b.isOccupiedByOpponent(position + dir, color))
				result = new CaptureMove(position, position + dir,
					b.castlingRights) :: result
			else
				if (b.isEmpty(position + dir))
					result = new QuietMove(position, position + dir, 0, 0,
						b.castlingRights) :: result
		})
		result
	}

	/* TODO: Implement */
	def rank : Int = 0
}


