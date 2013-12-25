package src;

class QuietMove(start : Int, end: Int, enPass : Int,
	castlingRightsAfterMove : Seq[Boolean]) 
	extends Move(Move.QUIET_MOVE, start, end, enPass,
	castlingRightsAfterMove)
{
	// see in Move
	override def apply(b : Board) =
	{
		val pieceID = b.board(start)
		val piece = b.piecesList(pieceID)
		b.board(start) = Board.EMPTY_SQUARE
		b.board(end) = pieceID

		piece.position = end

		b.castlingRights = castlingRightsAfter
		b.enPassant = enPassant
	}

	// see in Move
	override def undo(b : Board) : Unit = 
	{
		val pieceID = b.board(end)
		val piece = b.piecesList(pieceID)
		b.board(end) = Board.EMPTY_SQUARE
		b.board(start) = pieceID

		piece.position = start
	}

}
