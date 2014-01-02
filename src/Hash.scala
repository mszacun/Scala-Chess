package src

import scala.util.Random

// WARNING: Because of lack of unisgned number types iin JVM, we have to
// shift hashcode left, to avoid negative numbers, so we effectively use only 
// 63 bits of hashcode =[

class TranspositionTable(val size : Int)
{
	class TranspositionTableEntry(val key : Long, val move : Move)
	{
	}

	final val table = new Array[TranspositionTableEntry](size)

	final def get(key : Long) : Move =
	{
		val index : Int = ((key >>> 1) % size).toInt
		if (table(index) != null && table(index).key == key)
			return table(index).move
		else
			return null
	}

	final def set(key : Long, value : Move) : Unit =
	{
		val index : Int = ((key >>> 1) % size).toInt
		table(index) = new TranspositionTableEntry(key, value)
	}
}

	

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
