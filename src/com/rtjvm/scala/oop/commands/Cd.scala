package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {
  override def apply(state: State): State = {
    // Step 1: Find ROOT
    val root = state.root
    val workingDir = state.workingDirectory

    // Step 2: Find the absolute path of the directory
    val absolutePath = {
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (workingDir.isRoot) workingDir.path + dir
      else workingDir.path + Directory.SEPARATOR + dir
    }

    // Step 3: Find the directory that we need to cd to
    val destDir : DirEntry = findDestinationEntry(root, absolutePath)

    // Step 4: Change the state
    if (destDir == null || !destDir.isDirectory)
      state.setMessage(s"$dir: No such directory..!")
    else
      State(root, destDir.asDirectory)

  }

  // Find the Destination directory object
  def findDestinationEntry(root: Directory, path: String): DirEntry = {

    @tailrec
    def findEntryHelper(curDirectory: Directory, tokens: List[String]): DirEntry = {
      if (tokens.isEmpty || tokens.head.isEmpty) curDirectory
      else if (tokens.tail.isEmpty)  curDirectory.findEntry(tokens.head)
      else {
        val nextDir = curDirectory.findEntry(tokens.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, tokens.tail)
      }
    }

    @tailrec
    def collapseTokens(tokens: List[String], accumulator: List[String]): List[String] = {
      if (tokens.isEmpty) accumulator
      else if (tokens.head.equals(".")) collapseTokens(tokens.tail, accumulator)
      else if (tokens.head.equals("..")){
        if (accumulator.isEmpty) null
        else collapseTokens(tokens.tail, accumulator.init)
      }
      else collapseTokens(tokens.tail, accumulator :+ tokens.head)
    }

    // Step 1: Get the Token list of the path
    val tokens : List[String] = path.substring(1).split(Directory.SEPARATOR).toList

    // Step 1.5: Eliminate Relative Tokens
    /*
    (i) ./ => Will have no impact if we remove
    (ii) ../ => We need to migrate to parent dir
    */
    val cleanedTokens = collapseTokens(tokens, List())

    // Step 2: Navigate to the correct entry
    if (cleanedTokens == null) null
    else findEntryHelper(root, cleanedTokens)
  }
}
