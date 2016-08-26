package xyz.hyperreal.ramfile

import java.io.{IOException, Closeable, DataInput, DataOutput}
import java.nio.ByteBuffer


class RamFile extends Closeable {// with DataInput with DataOutput {
	val data = new ExpandableByteBuffer
	var closed = false
	
	def close {
		if (closed)
			throw new IOException( "closed" )
		else
			closed = true
	}
	
	def length: Long = data.size
	
	def getFilePointer: Long = data.buffer.position
	
	def readFully( b: Array[Byte] ) {
		data.getting( b.length )
		data.buffer.get( b )
	}
	
	def readInt = {
		data.getting( 4 )
		data.buffer.getInt
	}
	
	def readUnsignedShort = {
		data.getting( 2 )
		data.buffer.getShort&0xFFFF
	}
	
	def readUTF = {
		val len = readUnsignedShort
		val b = new Array[Byte]( len )
		
		readFully( b )
		new String( io.Codec.fromUTF8(b) )
	}
	
	def seek( p: Long ) {
		assert( p <= data.size, "file pointer must be less than or equal to file size" )
		data.buffer.position( p.asInstanceOf[Int] )
	}
	
	def setLength( l: Long ) = data.size = l.asInstanceOf[Int]
	
	def write( b: Array[Byte] ) {
		data.putting( b.length )
		data.buffer.put( b )
	}
	
	def writeInt( v: Int ) {
		data.putting( 4 )
		data.buffer.putInt( v )
	}
	
	def writeShort( v: Int ) {
		data.putting( 2 )
		data.buffer.putShort( v.asInstanceOf[Short] )
	}
	
	def writeUTF( s: String ) {
		val b = io.Codec.toUTF8( s )
		
		writeShort( b.length )
		write( b )
	}
}