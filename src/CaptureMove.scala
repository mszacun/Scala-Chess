package src;

class CaptureMove(startPos : Int, endPos : Int, 
	castlingRightsAfter : Seq[Boolean]) extends Move(startPos, endPos, 0, 0,
	castlingRightsAfter)
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
			b.enPassant1 = 0
			b.enPassant2 = 0
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
