package src;

class PromotionMove(start : Int, end : Int, castlingRightsAfterMove : Int, val promotion : Int) 
	extends Move(Move.PROMOTION_MOVE, start, end, 0, castlingRightsAfterMove)
{
	// pawn that was promoted, will be set during applying to board
	var pawnPromoted : Piece = null

	override def apply(b : Board) = 
	{
		val pieceID = b.board(start)
		pawnPromoted = b.piecesList(pieceID)
		b.scores(pawnPromoted.color) -= pawnPromoted.rank(b)

		// move it
		b.board(start) = Board.EMPTY_SQUARE
		b.board(end) = pieceID
		
		// change data in Board.is* arrays
		Board.isPawn(pieceID) = false
		
		// promote it
		
		val newPiece = promotion match
		{
			case Piece.QUEEN => 
				{ Board.isQueen(pieceID) = true; new Queen(end, pawnPromoted.color, pieceID) }
			case Piece.KNIGHT => 
				{ Board.isKnight(pieceID) = true; new Knight(end, pawnPromoted.color, pieceID) }
			case Piece.BISHOP => 
				{ Board.isBishop(pieceID) = true; new Bishop(end, pawnPromoted.color, pieceID) }
			case Piece.ROOK => 
				{ Board.isRook(pieceID) = true; new Rook(end, pawnPromoted.color, pieceID) }
		}
		b.piecesList(pieceID) = newPiece

		b.castlingRights &= castlingRightsMask
		castlingRightsAfter = b.castlingRights
		b.enPassant = enPassant

		b.scores(newPiece.color) += newPiece.rank(b)
	}

	override def undo(b : Board) = 
	{
		val pieceID = pawnPromoted.id
		val newPiece = b.piecesList(pieceID)

		b.scores(newPiece.color) -= newPiece.rank(b)
		
		b.board(end) = Board.EMPTY_SQUARE
		b.board(start) = pieceID
		b.piecesList(pieceID) = pawnPromoted

		Board.isPawn(pieceID) = true
		
		promotion match
		{
			case Piece.QUEEN => Board.isQueen(pieceID) = false
			case Piece.KNIGHT => Board.isKnight(pieceID) = false
			case Piece.BISHOP => Board.isBishop(pieceID) = false
			case Piece.ROOK => Board.isRook(pieceID) = false
		}

		b.scores(pawnPromoted.color) += pawnPromoted.rank(b)
	}

	override def calculateScore(b : Board) = 
		score = 20000 // default promotion score
}
