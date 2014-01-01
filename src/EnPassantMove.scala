package src;

class EnPassantMove(start : Int, end : Int, val captureField : Integer, 
	castlingRightsAfterMove : Int) extends Move(Move.ENPASSANT_MOVE,
	start, end, 0, castlingRightsAfterMove)
{
		var capturedPiece : Piece = null 
		var capturedPieceID : Int = -1
		var previousClock = 0

		override def apply(b : Board) =
		{
			val pieceID = b.board(start)
			val capturingPiece = b.piecesList(pieceID)
			capturedPieceID = b.board(captureField)
			capturedPiece = b.piecesList(capturedPieceID)

			b.scores(capturedPiece.color) -= capturedPiece.rank(b)
			b.scores(capturingPiece.color) -= capturingPiece.rank(b)
			
			b.board(end) = pieceID
			b.board(start) = Board.EMPTY_SQUARE
			b.board(captureField) = Board.EMPTY_SQUARE
			
			capturingPiece.position = end
			b.piecesList(capturedPieceID) = null

			b.castlingRights &= castlingRightsMask
			castlingRightsAfter = b.castlingRights
			b.enPassant = 0
			b.numberOfPiecesAlive -= 1
			
			b.scores(capturingPiece.color) += capturingPiece.rank(b)

			previousClock = b.halfMoveClock
			b.halfMoveClock = 0
		}

		override def undo(b : Board) = 
		{
			val pieceID = b.board(end)
			val capturingPiece = b.piecesList(pieceID)
			
			b.scores(capturingPiece.color) -= capturingPiece.rank(b)

			b.board(start) = pieceID
			b.board(captureField) = capturedPieceID
			b.board(end) = Board.EMPTY_SQUARE

			capturingPiece.position = start
			b.piecesList(capturedPieceID) = capturedPiece
			b.numberOfPiecesAlive += 1
			
			b.scores(capturingPiece.color) += capturingPiece.rank(b)
			b.scores(capturedPiece.color) += capturedPiece.rank(b)

			b.halfMoveClock = previousClock
		}

		override def calculateScore(b : Board) =
			score = 0 + 10000

}
