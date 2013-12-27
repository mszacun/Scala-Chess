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
		var possibleAttacks : List[Int] = Nil
		var lastRow : Int = 0 // row on which pawn of given color will be promoted
		if (color == Piece.WHITE)
		{
			lastRow = 7
			possibleAttacks = Cord.moveSE(position, 1) :: 
				Cord.moveSW(position, 1) :: Nil
		}
		else
		{
			lastRow = 0
			possibleAttacks = Cord.moveNE(position, 1) :: 
				Cord.moveNW(position, 1) :: Nil
		}
		possibleAttacks.foreach((pos : Int) =>
		{
			if (b.isOccupiedByOpponent(pos, color))
				if (Cord.getRow(pos) == lastRow)
					Pawn.possiblePromotions.foreach((pieceType : Int) =>
						result += new CapturePromotionMove(position, pos,
							b.castlingRights, pieceType))
				else
					result += new CaptureMove(position, pos, b.castlingRights)
		})
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
