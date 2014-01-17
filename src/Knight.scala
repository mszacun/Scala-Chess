package src;

import scala.collection.mutable.MutableList

object Knight
{
	final val possibleMovesDirection = Array(-19, -21, -12, -8, 19, 21, 8, 12)

	final val pieceValue = 320

	// position value tables and piece values taken from
	// http://chessprogramming.wikispaces.com/Simplified+evaluation+function
	final val positionValue = Array(
		Array(
			-50,-40,-30,-30,-30,-30,-40,-50,
			-40,-20,  0,  0,  0,  0,-20,-40,
			-30,  0, 10, 15, 15, 10,  0,-30,
			-30,  5, 15, 20, 20, 15,  5,-30,
			-30,  0, 15, 20, 20, 15,  0,-30,
			0,  5, 10, 15, 15, 10,  5,-30,
			-40,-20,  0,  5,  5,  0,-20,-40,
			-50,-40,-20,-30,-30,-20,-40,-50),
		Array(
			-50,-40,-20,-30,-30,-20,-40,-50,
			-40,-20,  0,  5,  5,  0,-20,-40,
			0,  5, 10, 15, 15, 10,  5,-30,
			-30,  0, 15, 20, 20, 15,  0,-30,
			-30,  5, 15, 20, 20, 15,  5,-30,
			-30,  0, 10, 15, 15, 10,  0,-30,
			-40,-20,  0,  0,  0,  0,-20,-40,
			-50,-40,-30,-30,-30,-30,-40,-50)
		)

}

class Knight(pos : Int, col : Int, identifire : Int)
	extends Piece(pos, col, identifire, Piece.KNIGHT)
{
	
	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	def generateMoves(b : Board, moveList : Array[Move], index : Int) = 
	{
		var ind = index
		var i = 0
		var tmpPos = 0
		val len = Knight.possibleMovesDirection.size

		while (i < len)
		{
			tmpPos = position + Knight.possibleMovesDirection(i)
			if (b.isEmpty(tmpPos))
			{
				moveList(ind) = new QuietMove(position, tmpPos, 0, b.castlingRights, false)
				ind += 1
			}
			else if (b.isOccupiedByOpponent(tmpPos, color))
			{
				moveList(ind) = new CaptureMove(position, tmpPos, b.castlingRights)
				ind += 1
			}
			i += 1
		}
		ind
	}

	def generateAttacks(b : Board, moveList : Array[Move], index : Int) = 
	{
		var ind = index
		var i = 0
		var tmpPos = 0
		val len = Knight.possibleMovesDirection.size

		while (i < len)
		{
			tmpPos = position + Knight.possibleMovesDirection(i)
			if (b.isOccupiedByOpponent(tmpPos, color))
			{
				moveList(ind) = new CaptureMove(position, tmpPos, b.castlingRights)
				ind += 1
			}
			i += 1
		}
		ind
	}

	def rank(b : Board) = Knight.pieceValue + 
		Knight.positionValue(color)(Cord.from120to64(position))
}


