package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.Directory
import com.rtjvm.scala.oop.filesystem.State

class Rm(name: String) extends Command {
  override def apply(state: State): State = {
    // Step 1: Get the Working Directory
    val workingDir = state.workingDirectory
    // Step 2: Get the absolute path of the entry to remove
    val absolutePath = {
      if (name.startsWith(Directory.SEPARATOR)) {
        name
      } else if (workingDir.isRoot) {
        workingDir.path + name
      } else {
        workingDir.path + Directory.SEPARATOR + name
      }
    }
    // Step 3: Do some checks
    if (absolutePath.equals(Directory.ROOT_PATH)) {
      state.setMessage("Root Dir can't be removed..!")
    } else {
      doRm(state, absolutePath)
    }
  }


  def doRm(state: State, path: String): State = {
    // Step 4: Find the Entry to remove
    // Step 5: Update the structure
    def rmHelper(curDir: Directory, tokens: List[String]): Directory = {
      if (tokens.isEmpty) {
        curDir
      } else if (tokens.tail.isEmpty) {
        curDir.removeEntry(tokens.head)
      } else {
        val nextDir = curDir.findEntry(tokens.head)
        if (!nextDir.isDirectory) {
          curDir
        } else {
          val newNextDir = rmHelper(nextDir.asDirectory, tokens.tail)
          if (newNextDir == nextDir) {
            curDir
          } else {
            curDir.replaceEntry(tokens.head, newNextDir)
          }
        }
      }
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)
    if (newRoot == state.root) {
      state.setMessage(s"$path : No such file or directory")
    } else {
      State(newRoot, newRoot.findDescendant(state.workingDirectory.path.substring(1)))
    }
  }
}
