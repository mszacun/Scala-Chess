package src;

class EnPassantMove(start : Int, end : Int, val captureField : Integer, 
	castlingRightsAfterMove : Seq[Boolean]) extends Move(Move.ENPASSANT_MOVE,
	start, end, 0, castlingRightsAfterMove)
{
		var capturedPiece : Piece = null 
		var capturedPieceID : Int = -1

		override def apply(b : Board) =
		{
			val pieceID = b.board(start)
			capturedPieceID = b.board(captureField)
			capturedPiece = b.piecesList(capturedPieceID)

			b.board(end) = pieceID
			b.board(start) = Board.EMPTY_SQUARE
			b.board(captureField) = Board.EMPTY_SQUARE
			
			b.piecesList(pieceID).position = end
			b.piecesList(capturedPieceID) = null

			b.castlingRights = castlingRightsAfter
			b.enPassant = 0
			b.numberOfPiecesAlive -= 1
		}

		override def undo(b : Board) = 
		{
			val pieceID = b.board(end)

			b.board(start) = pieceID
			b.board(captureField) = capturedPieceID
			b.board(end) = Board.EMPTY_SQUARE

			b.piecesList(pieceID).position = start
			b.piecesList(capturedPieceID) = capturedPiece
			b.numberOfPiecesAlive += 1
		}

		override def calculateScore(b : Board) =
			score = 0 + 10000

}
