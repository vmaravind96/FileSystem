package com.rtjvm.scala.oop.filesystem

import java.util.Scanner

import com.rtjvm.scala.oop.commands.Command
import com.rtjvm.scala.oop.files.Directory

object FileSystem extends App {

  // Get a new ROOT directory
  val root = Directory.ROOT
  // Initialize the First State
  var state = State(root, root)
  val scanner = new Scanner(System.in)

  while (true) {
    state.show()
    val input = scanner.nextLine()
    state = Command.from(input).apply(state)
  }

}


/*
************************************
Functional Way :
************************************
*
io.Source.stdin.getLines().foldLeft(State(root, root))((currState, newLine) => {
  currState.show()
  Command.from(newLine).apply(currState)
}
  )
*/