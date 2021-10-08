/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s): Adam Prieto and Alexander Sanford
 * Description: Prg 01 - LexicalAnalyzer (an iterable lexical analyzer)
 */

import LexicalAnalyzer.{BLANKS, DIGITS, LETTERS, NEW_LINE, PUNCTUATIONS, SPECIALS}
import scala.io.Source

class LexicalAnalyzer(private var source: String) extends Iterable[Lexeme]{

  var input = ""
  for (line <- Source.fromFile(source).getLines)
    input += line + NEW_LINE
  input = input.trim
  
  
  
  // checks if reached eof
  private def eof: Boolean = {
    input.length == 0
  } // End eof

  // returns the current char (requires checking for eof before call)
  private def getChar(): Char = {
    input(0)
  } // End getChar

  // advances the input one character (requires checking for eof before call)
  private def nextChar() = {
    input = input.substring(1)
  } // End nextChar

  // checks if input has a blank character ahead
  private def hasBlank(): Boolean = {
    BLANKS.contains(getChar)
  } // End hasBlank

  // reads the input until a non-blank character is found, updating the input
  def readBlanks: Unit = {
    var foundNonBlank = false
    while (!eof && !foundNonBlank) {
      val c = getChar
      if (hasBlank)
        nextChar
      else
        foundNonBlank = true
    } // End while loop
  } // End readBlanks

  // checks if input has a letter ahead
  private def hasLetter(): Boolean = {
    LETTERS.contains(getChar)
  } // End hasLetter

  // checks if input has a digit ahead
  private def hasDigit(): Boolean = {
    DIGITS.contains(getChar)
  } // End hasDigit

  // checks if input has a special character ahead
  private def hasSpecial(): Boolean = {
    SPECIALS.contains(getChar)
  } // End hasSpecial

  // checks if input has a punctuation character ahead
  private def hasPunctuation(): Boolean = {
    PUNCTUATIONS.contains(getChar)
  } // End hasPunctuation

  // returns an iterator for the lexical analyzer
  override def iterator: Iterator[Lexeme] = {

    new Iterator[Lexeme] {

      // returns true/false depending whether there is a lexeme to be read from the input
      override def hasNext: Boolean = {
        readBlanks
        !eof
      } // End hasNext


      // returns the next lexeme (or end of line if there isn't any lexeme left to be read)
      // TODOd: finish this part of the code
      override def next(): Lexeme = {

        // Check if the file has no remaining characters
        if (!hasNext)
          return new Lexeme("eof", Token.EOF)

        readBlanks
        var c = getChar
        var str = c + ""

        // EXTRA CREDIT
        // Support for comments —Completed
        while (getChar() == '\'')
          {
            while(getChar() != '\n' && !eof)
              {
                nextChar()
              } // End inner while
            nextChar()
            c = getChar()
          } // End outer while


        // Check if end of program
        if(str == "$")
          {
            nextChar()
            if(str == "$")
              {
                nextChar()
                return new Lexeme("$$", Token.EO_PRG)
             } // End second if
          } // End first if

        // Check if character is a letter
        else if (hasLetter) {
          nextChar
          var str = c + ""
          while (!eof && hasLetter() && getChar() != '\n') {
            str += getChar()
            nextChar
          }
          return new Lexeme(str, Token.IDENTIFIER)
        } // End if

        // Check if character is a digit
        else if (hasDigit())
          {
            nextChar
            var str = c + ""
            var done = false
            if(c == 0) return new Lexeme(str, Token.LITERAL)
            else
              {
                while(!done)
                  {
                    if(eof) done = true
                    else
                      {
                        c = getChar
                        if(hasDigit())
                          {
                            str += c
                            nextChar
                          } // End if
                        else done = true
                      } // End else
                  } // End while
                return new Lexeme(str, Token.LITERAL)
              } // End else
          } // End else if

          // Begin token checks
          // Plus token
          else if (c == '+') {
            val str = c + ""
            nextChar
            return new Lexeme(str, Token.PLUS)
          } // End else if

          // Minus token
          else if (c == '-')
            {
              val str = c + ""
              nextChar
              return new Lexeme(str, Token.MINUS)
            } // End else if

          // Question token
          else if (c == '?') {
              val str = c + ""
              nextChar
              return new Lexeme(str, Token.QUESTION)
            } // End else if

          // Equals token
          else if (c == '=') {
              val str = c + ""
              nextChar
              return new Lexeme(str, Token.EQUALS)
            } // End else if

          // Exclamation token
          else if (c == '!') {
              val str = c + ""
              nextChar
              return new Lexeme(str, Token.EXCLAMATION)
            } // End else if

          // Multiplication token
          else if (c == '*') {
              val str = c + ""
              nextChar
              return new Lexeme(str, Token.MULTIPLY)
            } // End else if

          // Divide token
          else if (c == '/') {
            val str = c + ""
            nextChar
            return new Lexeme(str, Token.DIVIDE)
          } // End else if

          // Modulo token
          else if (c == '\\') {
            val str = c + ""
            nextChar
            return new Lexeme(str, Token.MODULO)
          } // End else if

          // Break token
          else if (c == '^')
            {
              val str = c + ""
              nextChar
              return new Lexeme(str, Token.BREAK)
            } // End else if

          // Dot token
          else if (c == '.')
            {
              val str = c + ""
              nextChar
              return new Lexeme(str, Token.DOT)
            } // End else if

          // First Bracket token
          else if (c == '[')
            {
              val str = c + ""
              nextChar
              return new Lexeme(str, Token.OPEN_BRACKET)
            } // End else if

          // Second Bracket token
          else if (c == ']')
            {
              val str = c + ""
              nextChar
              return new Lexeme(str, Token.CLOSE_BRACKET)
            } // End else if

          // First Par token
          else if (c == '(') {
            val str = c + ""
            nextChar
            return new Lexeme(str, Token.OPEN_PAR)
          } // End else if

          // Second Par token
          else if (c == ')') {
            val str = c + ""
            nextChar
            return new Lexeme(str, Token.CLOSE_PAR)
          } // End else if

          // Double quote token
          else if (c == '"') {
            var string = ""
            nextChar
            while (!eof && getChar() != '\n' && getChar() != '"')
              {
                string += getChar()
                nextChar
              } // End while
            if (getChar() == '"')
            {
              nextChar
              return new Lexeme(string, Token.STRING)
            } // End if
            else
            {
              throw new Exception("Lexical Analyzer error: Closing '\"' expected!")
            } // End else
          } // End else if


        // throw an exception if an unrecognizable symbol is found
        println("Unrecognized Symbol: " + c)
        throw new Exception("Lexical Analyzer Error: unrecognizable symbol found!")
      } // End next
    } // End Iterator[Lexeme]
  } // End iterator: Iterator[Lexeme]
} // End class

object LexicalAnalyzer {
  val BLANKS       = " \n\t"
  val NEW_LINE     = '\n'
  val LETTERS      = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS       = "0123456789"
  val PUNCTUATIONS = ".,;:?!"
  val SPECIALS     = "<_@#$%^&()-+='/\\[]{}|"

  def main(args: Array[String]): Unit = {
    // checks if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    } // End if

    // iterates over the lexical analyzer, printing the lexemes found
    val lex = new LexicalAnalyzer(args(0))
    val it = lex.iterator
    while (it.hasNext)
      println(it.next())

  } // End main
} // End object
