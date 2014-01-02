package src;

class QuietMove(start : Int, end: Int, enPass : Int,
	castlingRightsAfterMove : Int, val resetClock : Boolean) 
	extends Move(Move.QUIET_MOVE, start, end, enPass,
	castlingRightsAfterMove)
{
	var previousClock : Int = 0 // state of half-move clock before move
	// see in Move
	override def apply(b : Board) =
	{
		val pieceID = b.board(start)
		val piece = b.piecesList(pieceID)

		// update player score after this move
		b.scores(piece.color) -= piece.rank(b)

		b.board(start) = Board.EMPTY_SQUARE
		b.board(end) = pieceID

		piece.position = end

		b.scores(piece.color) += piece.rank(b)

		b.castlingRights &= castlingRightsMask
		castlingRightsAfter = b.castlingRights
		b.enPassant = enPassant

		if (resetClock)
		{
			previousClock = b.halfMoveClock
			b.halfMoveClock = 0
		}
	}

	// see in Move
	override def undo(b : Board) : Unit = 
	{
		val pieceID = b.board(end)
		val piece = b.piecesList(pieceID)
		b.board(end) = Board.EMPTY_SQUARE
		b.board(start) = pieceID

		b.scores(piece.color) -= piece.rank(b)

		piece.position = start

		b.scores(piece.color) += piece.rank(b)

		if (resetClock)
			b.halfMoveClock = previousClock
	}

	override def calculateScore(b : Board) = 
		score = 0 // default score for quiet move

}
