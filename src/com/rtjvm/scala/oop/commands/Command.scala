package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State

trait Command extends (State => State) {

}

object Command {

  val MKDIR = "mkdir"
  val LS = "ls"
  val PWD = "pwd"
  val TOUCH = "touch"
  val CD = "cd"
  val RM = "rm"
  val ECHO = "echo"
  val CAT = "cat"

  // Gets the Command from the User and process it
  def from(input: String): Command = {
    val tokens: List[String] = input.split(" ").toList
    tokens.head match {
      case "" => emptyCommand
      case MKDIR =>
        if (tokens.length < 2) {
          incompleteCommand(MKDIR)
        } else {
          new MkDir(tokens.tail.head)
        }
      case LS => new Ls
      case PWD => new Pwd
      case TOUCH =>
        if (tokens.length < 2) {
          incompleteCommand(TOUCH)
        } else {
          new Touch(tokens.tail.head)
        }
      case CD =>
        if (tokens.length < 2) {
          incompleteCommand(CD)
        } else {
          new Cd(tokens.tail.head)
        }
      case RM =>
        if (tokens.length < 2) {
          incompleteCommand(RM)
        } else {
          new Rm(tokens.tail.head)
        }
      case ECHO =>
        if (tokens.length < 2) {
          incompleteCommand(ECHO)
        } else {
          new Echo(tokens.tail.toArray)
        } // We need to pass all the rest tokens
      case CAT =>
        if (tokens.length < 2) {
          incompleteCommand(CAT)
        } else {
          new Cat(tokens.tail.head)
        }
      case _ => new UnknownCommand
    }
  }

  // Empty Command means no command given. Return Same state
  def emptyCommand: Command = new Command {
    override def apply(state: State): State = state
  }

  def incompleteCommand(name: String): Command = new Command {
    override def apply(state: State): State =
      state.setMessage(s"$name command is incomplete!!")
  }
}