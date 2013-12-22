package src;

class QuietMove(startPos : Int, endPos: Int, enPassant1 : Int,
	enPassant2 : Int, castlingRightsAfter : Seq[Boolean]) 
	extends Move(startPos, endPos, enPassant1, enPassant2, castlingRightsAfter)
{
	// see in Move
	override def apply(b : Board) =
	{
		val pieceID = b.board(start)
		val piece = b.piecesList(pieceID)
		b.board(start) = Board.EMPTY_SQUARE
		b.board(end) = pieceID

		// TOCHECK: If position will be changed in pieceList also
		piece.position = end

		b.castlingRights = castlingRightsAfter
		b.enPassant1 = enPassant1
		b.enPassant2 = enPassant2
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
