/*
 * WORK THIS FIRST 9/28
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s): Adam Prieto
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
      // TODO: finish this part of the code
      override def next(): Lexeme = {

        if (!hasNext)
          return new Lexeme("eof", Token.EOF)

        readBlanks
        var c = getChar
        var str = c + ""

        // Check if end of file
        if(str == "$")
          {
            nextChar()
            if(str == "$")
              {
                nextChar()
                return new Lexeme("$$", Token.EO_PRG)
             } // End second if
          } // End first if



        // throw an exception if an unrecognizable symbol is found
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
