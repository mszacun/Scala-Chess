package src;

class Rook(position : Int, color : Int, id : Int)
	extends Piece(position, color, id, Piece.ROOK)
{
	// startingPos - position on which this certain root is placed at the beginning
	// of the game
	// posible move direction for rook
	

	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	def generateDirectionMoves(b : Board, acc : List[Move], dir : Int) :List[Move] = 
	{
		var tmpPos = position
		var result = acc

		// calculate castlingRights after this rook move
		// index of castling in board.castilngRights array, that will be impossible
		// after move of this rook
		val castleDeniedAfterMove = Cord.toString(position) match
		{
			case "H1" => 0
			case "A1" => 1
			case "H8" => 2
			case "A8" => 3
			case _ => 4
		}

		val castlingRightsAfter = Array(b.castlingRights(0), b.castlingRights(1),
			b.castlingRights(2), b.castlingRights(3), false)
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
		Rook.possibleDirections.foreach((dir : Int) => 
			result = generateDirectionMoves(b, result, dir))

		result
	}

	/* TODO: Implement */
	override def rank : Int = 0
}

object Rook
{
	val possibleDirections = Array(1, -1, 10, -10)
}
