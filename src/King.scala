package src;

import scala.collection.mutable.MutableList

class King(pos : Int, col : Int, identifier : Int)
	extends Piece(pos, col, identifier, Piece.KING)
{

	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	override def generateMoves(b : Board, moveList : Array[Move], index : Int) = 
	{
		var ind = index

		// if you move king, you loose all castling rights
		var castlingRightsAfter : Int = 0
		if (color == Piece.WHITE)
			castlingRightsAfter = 12
		else
			castlingRightsAfter = 3 

		King.possibleDirections.foreach((dir : Int) => 
		{
			val tmpPos = position + dir
			if (b.isEmpty(tmpPos))
			{
				moveList(ind) = new QuietMove(position, tmpPos, 0, castlingRightsAfter, false)
				ind += 1
			}
			else
				if (b.isOccupiedByOpponent(tmpPos, color))
				{
					moveList(ind) = new CaptureMove(position, tmpPos, castlingRightsAfter)
					ind += 1
				}
		})

		ind
	}

	override def generateAttacks(b : Board, moveList : Array[Move], index : Int) = 
	{
		var ind = index

		// if you move king, you loose all castling rights
		var castlingRightsAfter : Int = 0
		if (color == Piece.WHITE)
			castlingRightsAfter = 12
		else
			castlingRightsAfter = 3

		King.possibleDirections.foreach((dir : Int) => 
		{
			val tmpPos = position + dir
			if (b.isOccupiedByOpponent(tmpPos, color))
			{
				moveList(ind) = new CaptureMove(position, tmpPos, castlingRightsAfter)
				ind += 1
			}
		})

		ind
	}

	override def rank(b : Board) : Int = King.pieceValue + (b.numberOfPiecesAlive / 32) * 
		King.positionValueStartGame(color)(Cord.from120to64(position)) +
		((32 - b.numberOfPiecesAlive) / 32) * 
		King.positionValueEndGame(color)(Cord.from120to64(position)) 
}

object King
{
	final val possibleDirections = Array(9, 11, -9, -11, 1, -1, 10, -10)

	final val pieceValue = 32767

	// position value tables and piece values taken from
	// http://chessprogramming.wikispaces.com/Simplified+evaluation+function
	final val positionValueStartGame = Array[Array[Int]](
		Array[Int](
			-30, -40, -40, -50, -50, -40, -40, -30,
			-30, -40, -40, -50, -50, -40, -40, -30,
			-30, -40, -40, -50, -50, -40, -40, -30,
			-30, -40, -40, -50, -50, -40, -40, -30,
			-20, -30, -30, -40, -40, -30, -30, -20,
			-10, -20, -20, -20, -20, -20, -20, -10,
			 20,  20,   0,   0,   0,   0,  20,  20,
		     20,  30,  10,   0,   0,  10,  30,  20),
		Array[Int](
		     20,  30,  10,   0,   0,  10,  30,  20,
			 20,  20,   0,   0,   0,   0,  20,  20,
			-10, -20, -20, -20, -20, -20, -20, -10,
			-20, -30, -30, -40, -40, -30, -30, -20,
			-30, -40, -40, -50, -50, -40, -40, -30,
			-30, -40, -40, -50, -50, -40, -40, -30,
			-30, -40, -40, -50, -50, -40, -40, -30,
			-30, -40, -40, -50, -50, -40, -40, -30)
	)

	final val positionValueEndGame = Array(
		Array(
			-50,-40,-30,-20,-20,-30,-40,-50,
			-30,-20,-10,  0,  0,-10,-20,-30,
			-30,-10, 20, 30, 30, 20,-10,-30,
			-30,-10, 30, 40, 40, 30,-10,-30,
			-30,-10, 30, 40, 40, 30,-10,-30,
			-30,-10, 20, 30, 30, 20,-10,-30,
			-30,-30,  0,  0,  0,  0,-30,-30,
			-50,-30,-30,-30,-30,-30,-30,-50),
		Array(
			-50,-30,-30,-30,-30,-30,-30,-50,
			-30,-30,  0,  0,  0,  0,-30,-30,
			-30,-10, 20, 30, 30, 20,-10,-30,
			-30,-10, 30, 40, 40, 30,-10,-30,
			-30,-10, 30, 40, 40, 30,-10,-30,
			-30,-10, 20, 30, 30, 20,-10,-30,
			-30,-20,-10,  0,  0,-10,-20,-30,
			-50,-40,-30,-20,-20,-30,-40,-50)
	)
}
