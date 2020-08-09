package com.rtjvm.scala.oop.filesystem

import com.rtjvm.scala.oop.files.Directory

class State(val root: Directory, val workingDirectory: Directory, val output: String) {

  // Prints the Output/ message and the Shell Token
  def show(): Unit = {
    println(output)
    print(State.SHELL_TOKEN)
  }

  // Sets the message/ Output
  def setMessage(message: String): State =
    State(root, workingDirectory, message)

}

object State {
  val SHELL_TOKEN = "$ "

  // Creates a new State
  def apply(root: Directory, workingDirectory: Directory, output: String = ""): State = {
    new State(root, workingDirectory, output)
  }
}
