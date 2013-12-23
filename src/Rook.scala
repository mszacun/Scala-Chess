package src;

class Rook(position : Int, color : Int, id : Int, val startingPos : Int)
	extends Piece(position, color, id)
{
	// startingPos - position on which this certain root is placed at the beginning
	// of the game
	// posible move direction for rook
	val possibleDirections = Array(1, -1, 10, -10)
	
	// index of castling in board.castilngRights array, that will be impossible
	// after first move of this rook
	val castleDeniedAfterMove = 
		Cord.toString(startingPos) match
		{
			case "A1" => 1
			case "H1" => 0
			case "A8" => 3
			case "H8" => 2
		}

	def this(position : String, color : Int, id : Int, startingPos : String)= 
		this(Cord.fromString(position), color, id, Cord.fromString(startingPos))

	def generateDirectionMoves(b : Board, acc : List[Move], dir : Int) :List[Move] = 
	{
		var tmpPos = position
		var result = acc

		// calculate castlingRights after this rook move
		val castlingRightsAfter = Array(b.castlingRights(0), b.castlingRights(1),
			b.castlingRights(2), b.castlingRights(3))
		castlingRightsAfter(castleDeniedAfterMove) = false

		while (true)
		{
			tmpPos += dir
			if (b.isEmpty(tmpPos))
				result = new QuietMove(position, tmpPos, 0, 0, 
				castlingRightsAfter) :: result
			else
			{
				if (b.isOccupiedByOpponent(tmpPos, color))
					result = new CaptureMove(position, tmpPos,
						castlingRightsAfter) :: result
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
