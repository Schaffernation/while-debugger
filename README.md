while-debugger
==============

Joseph Schaffer (jschaf)
Noam Zilberstein (noamz)

This project is a debugger for the WHILE language we developed in class. To see
it in action, compile Main.hs and run the executable. Once its running, and the
debugger has started, just type `help` to get a list of commands available. The
debugger works like most debuggers, it has facilities for stepping, breakpoints,
state inspection and more. A user can load in a file (using `load filename.whl`)
and begin debugging. Included in the assignment are some sample WHILE programs
which can be found in the the *progs* folder. One recommendation is run the 
executable inside rlwrap like this: `rlwrap ./Main`. This adds readline support.

The project is spread across multiple components and files:

**Main.hs** 

  The main user interaction loop. This is where all running, stepping forward,
  stepping backward, breakpointing store state.

**WhilePP.hs**
**WhileSyntax.hs**

  These files are the AST type definitions for the meta representation of the 
  WHILE language. They also hold all the same necessary information for pretty 
  printing the while language. 


**ParserCombinators.hs**
**ParserTrans.hs**

  These files are the backbone of the parsers and tokenizers used to take in
  input. (Whether it be debugger command input of code input, these files 
  support it). Much of this is similar to what was worked on in class with,
  but added is some important state features. Now the parser can keep track of
  line numbers in addition to the string that is being parsed in. This is
  crucial in creating a refrence point for debugger users.

**CommandParser.hs**

  Parses commands users type into the debugger.

**WhileParser.hs**

  Parses the language itself from files, into tokens, and then into an AST which
  can be executed.

**WhileStep.hs**

  All the execution logic lives in this file. It creates a small-step semantics
  so that a given program can step line by line through a program. This is the
  logic that Main.hs uses to execute.

