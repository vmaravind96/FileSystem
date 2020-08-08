package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

class MkDir(name: String) extends CreateEntry(name) {

  override def doCreateSpecificEntry(state: State): DirEntry = {
    Directory.empty(parentPath = state.workingDirectory.path, name = name)
  }

}
