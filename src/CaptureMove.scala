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
			val capturingPiece = b.piecesList(pieceID)

			b.scores(capturedPiece.color) -= capturedPiece.rank(b)
			b.scores(capturingPiece.color) -= capturingPiece.rank(b)

			b.board(end) = pieceID
			b.board(start) = Board.EMPTY_SQUARE
			
			capturingPiece.position = end
			b.piecesList(capturedPieceID) = null

			b.scores(capturingPiece.color) += capturingPiece.rank(b)

			b.castlingRights = castlingRightsAfter
			b.enPassant = 0
			b.numberOfPiecesAlive -= 1
		}

		override def undo(b : Board) = 
		{
			val pieceID = b.board(end)
			val capturingPiece = b.piecesList(pieceID)

			b.scores(capturingPiece.color) -= capturingPiece.rank(b)

			b.board(start) = pieceID
			b.board(end) = capturedPieceID

			capturingPiece.position = start
			b.piecesList(capturedPieceID) = capturedPiece
			b.numberOfPiecesAlive += 1


			b.scores(capturedPiece.color) += capturedPiece.rank(b)
			b.scores(capturingPiece.color) += capturingPiece.rank(b)
		}
	}
