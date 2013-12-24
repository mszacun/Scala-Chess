package src;

import scala.collection.mutable.MutableList


class Pawn(position : Int, color : Int, id : Int) 
	extends Piece(position, color, id, Piece.PAWN)
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
					result += new PromotionMove(position, move, b.castlingRights,
						Piece.QUEEN) // FIXME: AI always will promote to queen!
				else
				{
					result += new QuietMove(position, move, 0, 0, b.castlingRights)

					// double move if on starting position
					if (Cord.getRow(position) == whitePawnsStartingRow)
					{
						move = Cord.moveS(position, 2)
						if (b.isEmpty(move) && !b.isOffBoard(move))
							result += new QuietMove(position, 
								move, Cord.moveSW(position, 1), // enPasant is possible
								Cord.moveSE(position, 1), b.castlingRights)
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
					result += new PromotionMove(position, move, b.castlingRights,
						Piece.QUEEN)
				else
				{
					result += new QuietMove(position, move, 0, 0, b.castlingRights)

					// double move if on starting position
					if (Cord.getRow(position) == blackPawnsStartingRow)
					{
						move = Cord.moveN(position, 2)
						if (b.isEmpty(move) && !b.isOffBoard(move))
							result += new QuietMove(position, 
								move, Cord.moveNW(position, 1),
								Cord.moveNE(position, 1), b.castlingRights)
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
