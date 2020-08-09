package com.rtjvm.scala.oop.files

import com.rtjvm.scala.oop.filesystem.FileSystemException

class File(override val parentPath: String, override val name: String, contents: String)
  extends DirEntry(parentPath, name) {

  override def asDirectory: Directory =
    throw new FileSystemException("File can't be converted to directory")

  override def asFile: File = this

  override def getType: String = "File"

  override def isDirectory: Boolean = false

  override def isFile: Boolean = true

}

object File {

  // Create an Empty file.
  def empty(parentPath: String, name: String): File = {
    new File(parentPath, name, "")
  }

}
