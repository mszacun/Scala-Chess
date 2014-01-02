package src;

class CaptureMove(start : Int, end : Int, 
	castlingRightsAfterMove : Seq[Boolean]) extends Move(Move.CAPTURE_MOVE,
	start, end, 0, castlingRightsAfterMove)
	{
		// will be assigned during applaying to board
		var capturedPiece : Piece = null 
		var capturedPieceID : Int = -1

		override def apply(b : Board) =
		{
			val pieceID = b.board(start)
			capturedPieceID = b.board(end)
			capturedPiece = b.piecesList(capturedPieceID)

			b.board(end) = pieceID
			b.board(start) = Board.EMPTY_SQUARE
			
			b.piecesList(pieceID).position = end
			b.piecesList(capturedPieceID) = null

			b.castlingRights = castlingRightsAfter
			b.enPassant = 0
		}

		override def undo(b : Board) = 
		{
			val pieceID = b.board(end)

			b.board(start) = pieceID
			b.board(end) = capturedPieceID

			b.piecesList(pieceID).position = start
			b.piecesList(capturedPieceID) = capturedPiece
		}
	}
