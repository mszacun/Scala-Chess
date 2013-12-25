package src;

class PromotionMove(start : Int, end : Int, castlingRightsAfterMove : Seq[Boolean], promotion : Int) 
	extends Move(Move.PROMOTION_MOVE, start, end, 0, castlingRightsAfterMove)
{
	// pawn that was promoted, will be set during applying to board
	var pawnPromoted : Piece = null

	override def apply(b : Board) = 
	{
		val pieceID = b.board(start)
		pawnPromoted = b.piecesList(pieceID)

		// move it
		b.board(start) = Board.EMPTY_SQUARE
		b.board(end) = pieceID
		
		// change data in Board.is* arrays
		Board.isPawn(pieceID) = false
		
		// promote it
		Board.isQueen(pieceID) = true
		val newPiece = promotion match
			{
				case Piece.QUEEN => new Queen(end, pawnPromoted.color, pieceID)
				case _ => throw new NotImplementedError
			}
		b.piecesList(pieceID) = newPiece

		b.castlingRights = castlingRightsAfter
		b.enPassant = enPassant
	}

	override def undo(b : Board) = 
	{
		val pieceID = pawnPromoted.id
		
		b.board(end) = Board.EMPTY_SQUARE
		b.board(start) = pieceID
		b.piecesList(pieceID) = pawnPromoted

		Board.isPawn(pieceID) = true
		Board.isQueen(pieceID) = false
	}
}
