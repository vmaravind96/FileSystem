package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State

trait Command {
  def apply(state: State): State
}

object Command {

  val MKDIR = "mkdir"
  val LS = "ls"
  val PWD = "pwd"
  val TOUCH = "touch"
  val CD = "cd"

  // Gets the Command from the User and process it
  def from(input: String): Command = {
    val tokens : Array[String] = input.split(" ")
    tokens(0) match {
      case "" => emptyCommand
      case MKDIR =>
        if (tokens.length < 2) incompleteCommand(MKDIR)
        else new MkDir(tokens(1))
      case LS => new Ls
      case PWD => new Pwd
      case TOUCH =>
        if (tokens.length < 2) incompleteCommand(TOUCH)
        else new Touch(tokens(1))
      case CD =>
        if (tokens.length < 2) incompleteCommand(CD)
        else new Cd(tokens(1))
      case _ => new UnknownCommand
    }
  }

  // Empty Command means no command given. Return Same state
  def emptyCommand : Command = new Command {
    override def apply(state: State): State = state
  }

  def incompleteCommand(name : String) : Command = new Command {
    override def apply(state: State): State =
      state.setMessage(s"$name command is incomplete!!")
  }
}