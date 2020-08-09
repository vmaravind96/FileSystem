package com.rtjvm.scala.oop.files

import com.rtjvm.scala.oop.filesystem.FileSystemException

import scala.annotation.tailrec

class Directory(override val parentPath: String, override val name: String, val contents : List[DirEntry])
  extends DirEntry(parentPath, name) {

  override def asDirectory: Directory = this

  override def asFile: File =
    throw new FileSystemException("A Directory cannot be converted to File..!")

  override def getType: String = "Directory"

  override def isDirectory: Boolean = true

  override def isFile: Boolean = false

  // Check if the Working Directory has an entry with name as specified
  def hasEntry(entryName: String): Boolean = {
    findEntry(entryName) != null
  }

  // Return all the folders that are in the current path (Working Dir)
  def getAllFoldersInPath : List[String] = {
    // Eg: /home/apple/repo/ => List("home", "apple", "repo")
    // Note : Here "path" is the method DirEntry.
    path.substring(1).split(Directory.SEPARATOR).toList.filter(x => !x.isEmpty)
  }

  // Find the descendant node
  def findDescendant(path: List[String]): Directory = {
    // Eg : /home/apple/repo and Path("home" , "apple")
    // Returns Apple Directory Entry
    if (path.isEmpty) this
    else findEntry(path.head).asDirectory.findDescendant(path.tail)
  }

  // Add a new DirEntry to current content list
  def addEntry(newEntry: DirEntry): Directory = {
    new Directory(parentPath, name, contents :+ newEntry)
  }

  // Finds an entry in the working directory with name specified
  def findEntry(entryName: String) : DirEntry = {
    @tailrec
    def findEntryHelper(entryName: String, contentList: List[DirEntry]): DirEntry = {
      if (contentList.isEmpty) null
      else if (contentList.head.name.equals(entryName)) contentList.head
      else findEntryHelper(entryName, contentList.tail)
    }
    findEntryHelper(entryName, contents)
  }

  // Replace Old Entry with new Entry
  def replaceEntry(oldName : String, newEntry: DirEntry): Directory = {
    new Directory(parentPath, name,
      contents.filter(entry => !entry.name.equals(oldName)) :+ newEntry)
  }

  // Check if directory is root
  def isRoot: Boolean = parentPath.isEmpty

}

object Directory {
  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  // Utility to create a new directory
  def empty(parentPath: String, name : String): Directory = {
    new Directory(parentPath, name, List())
  }

  // Utility to create ROOT directory
  def ROOT: Directory = Directory.empty("", "")
}
