package src;

class PromotionMove(start : Int, end : Int, castlingRightsAfter : Seq[Boolean], promotion : Int) 
	extends Move(Move.PROMOTION_MOVE, start, end, 0, 0, castlingRightsAfter)
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
		
		// promote it
		val newPiece = promotion match
			{
				case Piece.ROOK => new Rook(end, pawnPromoted.color, pieceID)
				case Piece.QUEEN => new Queen(end, pawnPromoted.color, pieceID)
				case Piece.BISHOP => new Bishop(end, pawnPromoted.color, pieceID)
				case Piece.KNIGHT => new Knight(end, pawnPromoted.color, pieceID)
			}
		b.piecesList(pieceID) = newPiece

		b.castlingRights = castlingRightsAfter
		b.enPassant1 = enPassant1
		b.enPassant2 = enPassant2
	}

	override def undo(b : Board) = 
	{
		val pieceID = pawnPromoted.id
		
		b.board(end) = Board.EMPTY_SQUARE
		b.board(start) = pieceID
		b.piecesList(pieceID) = pawnPromoted
	}
}
