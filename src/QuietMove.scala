package src;

class QuietMove(start : Int, end: Int, enPass : Int,
	castlingRightsAfterMove : Int, val resetClock : Boolean) 
	extends Move(Move.QUIET_MOVE, start, end, enPass,
	castlingRightsAfterMove)
{
	var previousLastUndoableMove = 0
	// see in Move
	override def apply(b : Board) =
	{
		val pieceID = b.board(start)
		val piece = b.piecesList(pieceID)

		b.board(start) = Board.EMPTY_SQUARE
		b.board(end) = pieceID

		piece.position = end

		b.castlingRights &= castlingRightsMask
		castlingRightsAfter = b.castlingRights
		b.enPassant = enPassant

		if (resetClock)
		{
			previousLastUndoableMove = b.lastUndoAbleMove
			b.lastUndoAbleMove = b.boardHashHistoryIndex
		}
	}

	// see in Move
	override def undo(b : Board) : Unit = 
	{
		val pieceID = b.board(end)
		val piece = b.piecesList(pieceID)
		b.board(end) = Board.EMPTY_SQUARE
		b.board(start) = pieceID

		piece.position = start

		if (resetClock)
			b.lastUndoAbleMove = previousLastUndoableMove
	}

	override def calculateScore(b : Board) = 
		score = 0 // default score for quiet move

}
