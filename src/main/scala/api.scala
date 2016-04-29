package jio {
	import java.nio.file._

	object Backup {
		def arrToList(p: Path) = if(Files.exists(p) && Files.isDirectory(p) &&
			p.toFile.listFiles != null) p.toFile.listFiles.toList.map(_.toPath) else List[Path]()
		
		def copyFile(from: Path, to: Path): Either[Exception, Path] = try Right(Files.copy(from, to, StandardCopyOption.REPLACE_EXISTING)) 
			catch { case e: Exception => Left(e)}
		
		def deleteAllFiles(p: Path): Either[Exception, Unit] = try Right(arrToList(p).map { (path: Path) => Files.delete(path) })
			catch { case e: Exception => Left(e)}
			
		def createDirectory(p: Path): Either[Exception, Path] = try Right(Files.createDirectory(p)) catch { case e: Exception => Left(e)}
	}
}