package jio {
	import java.nio.file._

	object File {
		/** This function takes a java.nio.file.Path and converts it to Scala List
		  * if it is a directory.  If the path object is null, or doesn't exist,
		  * the method returns an empty list.
		  */
		def arrToList(p: Path) = if(Files.exists(p) && Files.isDirectory(p) &&
			p.toFile.listFiles != null) p.toFile.listFiles.toList.map(_.toPath) else List[Path]()
		
		/** This function takes two java.nio.file.Path objects, and copies
		  * one Path from one location to the other.
		  * It returns either the new Path location or an Exception if thrown.
		  */
		def copyFile(from: Path, to: Path): Either[Exception, Path] = try Right(Files.copy(from, to, StandardCopyOption.REPLACE_EXISTING)) 
			catch { case e: Exception => Left(e)}
		
		/** This function deletes all non-directory files in a directory.
		  * It returns either a Unit or an exception if thrown.
		  */
		def deleteAllFiles(p: Path): Either[Exception, Unit] = try Right(arrToList(p).map { (path: Path) => Files.delete(path) })
			catch { case e: Exception => Left(e)}
		
		/** This function takes a java.nio.file.Path and creates a directory.
		  * It returns either the createDirectory Path or an exception if thrown.
		  */
		def createDirectory(p: Path): Either[Exception, Path] = try Right(Files.createDirectory(p)) catch { case e: Exception => Left(e)}
	}
}