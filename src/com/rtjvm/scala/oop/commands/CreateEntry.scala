package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

abstract class CreateEntry(name: String) extends Command {

  override def apply(state: State): State = {
    val workingDir = state.workingDirectory
    if (workingDir.hasEntry(name)) {
      state.setMessage(s"Entry $name already exists..")
    } else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(s"$name must not contain separators..!")
    } else if (checkIllegal(name)) {
      state.setMessage(s"$name is not a valid name..!")
    } else {
      doCreateEntry(state, name)
    }

  }

  // If name contains . (or) .. then it is illegal name
  def checkIllegal(str: String): Boolean = str.contains('.')

  // Create a Directory (or) File
  def doCreateEntry(state: State, name: String): State = {
    // Updates all the directories in the Full path
    def updateStructure(curDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) curDirectory.addEntry(newEntry)
      else {
        val oldEntry = curDirectory.findEntry(path.head).asDirectory
        curDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }

    val workingDir: Directory = state.workingDirectory
    // Step 1: Get all the existing Directories in Full Path
    val allDirsInPath = workingDir.getAllFoldersInPath
    // Step 2: Create a new Directory (or) File Entry in Working Directory
    val newEntry = doCreateSpecificEntry(state)
    // Step 3: Update the whole directory structure starting from Root
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)
    // Step 4: Find new WD in the new Directory Structure
    val newWD = newRoot.findDescendant(allDirsInPath)
    State(newRoot, newWD)
  }

  def doCreateSpecificEntry(state: State): DirEntry
}
