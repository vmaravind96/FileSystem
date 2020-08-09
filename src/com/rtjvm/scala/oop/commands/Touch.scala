package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, File}
import com.rtjvm.scala.oop.filesystem.State

class Touch(name: String) extends CreateEntry(name) {
  override def doCreateSpecificEntry(state: State): DirEntry = {
    File.empty(state.workingDirectory.path, name)
  }
}
