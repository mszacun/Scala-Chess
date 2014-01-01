package src

import scala.util.Random

object Hash
{
	// piecesHash[pieceID][position]
	final val piecesHash = new Array[Array[Long]](32)

	// castleRightsHash[castleRights]
	final val castleRightsHash = new Array[Long](16)

	final var whiteMovesHash : Long = 0

	final val enPassantSquareHash = new Array[Long](65)

	final def initHashTables = 
	{
		val r = new Random

		/* piece hashes */
		for (i <- 0 until 32)
		{
			piecesHash(i) = new Array[Long](65)
			for (j <- 0 until 65)
				piecesHash(i)(j) = r.nextLong
		}

		/* castle right hashes */
		for (i <- 0 until 16)
			castleRightsHash(i) = r.nextLong

		whiteMovesHash = r.nextLong

		for (i <- 0 until 65)
			enPassantSquareHash(i) = r.nextLong
	}
}	 