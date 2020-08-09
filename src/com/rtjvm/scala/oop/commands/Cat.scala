package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State

class Cat(fileName: String) extends Command {

  override def apply(state: State): State = {
    val workingDir = state.workingDirectory
    val dirEntry = workingDir.findEntry(fileName)
    if (dirEntry == null || !dirEntry.isFile) {
      state.setMessage(s"$fileName : no such file..!")
    } else {
      state.setMessage(dirEntry.asFile.getContent)
    }
  }

}
