package src;

import scala.collection.mutable.MutableList


class Pawn(pos : Int, col : Int, identifier : Int) 
	extends Piece(pos, col, identifier, Piece.PAWN)
{
	val whitePawnsStartingRow = 1 // remeber, that rows are 0 indexed
	val blackPawnsStartingRow = 6

	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	// see in Piece
	def generateQuietMoves(b : Board) : MutableList[Move] =
	{
		var result : MutableList[Move]= new MutableList[Move]

		if (color == Piece.WHITE)
		{
			// default move
			var move = Cord.moveS(position, 1)
			if (b.isEmpty(move) && !b.isOffBoard(move))
			{
				if (Cord.getRow(move) == 7)
					Pawn.possiblePromotions.foreach((pieceType : Int) =>
						result += new PromotionMove(position, move,
							b.castlingRights, pieceType))
				else
				{
					result += new QuietMove(position, move, 0, b.castlingRights)

					// double move if on starting position
					if (Cord.getRow(position) == whitePawnsStartingRow)
					{
						move = Cord.moveS(position, 2)
						if (b.isEmpty(move) && !b.isOffBoard(move))
							result += new QuietMove(position, 
								move, Cord.moveS(position, 1), // enPasant is possible
								b.castlingRights)
					}
				}
			}
		}
		else
		{
			// default move
			var move = Cord.moveN(position, 1)
			if (b.isEmpty(move) && !b.isOffBoard(move))
			{
				if (Cord.getRow(move) == 0)
					Pawn.possiblePromotions.foreach((pieceType : Int) =>
						result += new PromotionMove(position, move,
							b.castlingRights, pieceType))
				else
				{
					result += new QuietMove(position, move, 0, b.castlingRights)

					// double move if on starting position
					if (Cord.getRow(position) == blackPawnsStartingRow)
					{
						move = Cord.moveN(position, 2)
						if (b.isEmpty(move) && !b.isOffBoard(move))
							result += new QuietMove(position, 
								move, Cord.moveN(position, 1),
								b.castlingRights)
					}
				}
			}
		}
		result
	}

	def generateAttacks(b : Board) : MutableList[Move] =
	{
		var result : MutableList[Move] = new MutableList
		if (color == Piece.WHITE)
		{
			if (b.isOccupiedByOpponent(Cord.moveSE(position, 1), color))
				result += new CaptureMove(position, Cord.moveSE(position, 1),
					b.castlingRights)
			if (b.isOccupiedByOpponent(Cord.moveSW(position, 1), color))
				result += new CaptureMove(position, Cord.moveSW(position, 1),
					b.castlingRights)
		}
		else
		{
			if (b.isOccupiedByOpponent(Cord.moveNE(position, 1), color))
				result += new CaptureMove(position, Cord.moveNE(position, 1),
					b.castlingRights)
			if (b.isOccupiedByOpponent(Cord.moveNW(position, 1), color))
				result += new CaptureMove(position, Cord.moveNW(position, 1),
					b.castlingRights)
		}
		result 
	}
	override def generateMoves(b : Board) : MutableList[Move] =
		generateAttacks(b) ++ generateQuietMoves(b)

	/* TODO: Implement */
	def rank = 0

}

object Pawn
{
	val possiblePromotions = Piece.KNIGHT :: Piece.BISHOP :: Piece.QUEEN ::
		Piece.ROOK :: Nil
}
