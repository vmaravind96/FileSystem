package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.DirEntry
import com.rtjvm.scala.oop.filesystem.State

class Ls extends Command {

  override def apply(state: State): State = {
    val contents = state.workingDirectory.contents
    val output = createNiceOutput(contents)
    state.setMessage(output)
  }

  def createNiceOutput(contents: List[DirEntry]): String = {
    if (contents.isEmpty) {
      ""
    } else {
      val entry = contents.head
      s"${entry.name}[${entry.getType}]\n${createNiceOutput(contents.tail)}"
    }
  }

}
