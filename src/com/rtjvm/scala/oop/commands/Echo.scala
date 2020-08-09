package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{Directory, File}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {
  override def apply(state: State): State = {

    val argsLength = args.length
    argsLength match {
      case 0 => state
      case 1 => state.setMessage(args(0))
      case _ =>
        val operator = args(argsLength - 2)
        val fileName = args(argsLength - 1)
        val content = createContent(args, argsLength - 2)

        operator match {
          case ">>" => doEcho(state, content, fileName, isAppend = true)
          case ">" => doEcho(state, content, fileName, isAppend = false)
          case _ => state.setMessage(createContent(args, argsLength))
        }
    }
  }

  // Create Content excluding topIndex
  def createContent(args: Array[String], topIndex: Int): String = {

    @tailrec
    def createContentHelper(curIndex: Int = 0, accumulator: String = ""): String = {
      if (curIndex == topIndex) {
        accumulator
      } else {
        createContentHelper(curIndex + 1, s"$accumulator ${args(curIndex)}")
      }
    }

    createContentHelper()
  }

  // DoEcho
  def doEcho(state: State, content: String, fileName: String, isAppend: Boolean): State = {
    if (fileName.contains(Directory.SEPARATOR)) {
      state.setMessage("echo: File name must not contain separators")
    } else {
      val filePath: List[String] = state.workingDirectory.getAllFoldersInPath :+ fileName
      val newRoot: Directory = getRootAfterEcho(state.root, filePath, content, isAppend)
      if (newRoot == state.root) {
        state.setMessage(s"$fileName : no such file..!")
      } else {
        State(newRoot, newRoot.findDescendant(state.workingDirectory.getAllFoldersInPath))
      }
    }
  }

  // doEchoHelper
  def getRootAfterEcho(curDir: Directory, path: List[String], content: String, isAppend: Boolean): Directory = {
    if (path.isEmpty) {
      curDir
    } else if (path.tail.isEmpty) {
      val dirEntry = curDir.findEntry(path.head)
      if (dirEntry == null) {
        curDir.addEntry(new File(curDir.path, path.head, content))
      } else if (dirEntry.isDirectory) {
        curDir
      } else {
        if (isAppend) {
          curDir.replaceEntry(path.head, dirEntry.asFile.appendContent(content))
        } else {
          curDir.replaceEntry(path.head, dirEntry.asFile.setContent(content))
        }
      }
    }
    else {
      val nextDir = curDir.findEntry(path.head).asDirectory
      val newNextDir = getRootAfterEcho(nextDir, path.tail, content, isAppend)
      if (newNextDir == nextDir) {
        curDir
      } else {
        curDir.replaceEntry(path.head, newNextDir)
      }
    }
  }
}
