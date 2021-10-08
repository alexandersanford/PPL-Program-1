/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s): Adam Prieto and Alexander Sanford
 * Description: Prg 01 - SyntaxAnalyzer (an iterable syntax analyzer)
 */

/*
mouse       = { statement } ´$$´
statement   = ´?´ | ´!´ | string | identifier | ´=´ | literal | ´+´ | ´-´ |
              ´*´ | ´/´ | ´\´ | ´^´ | ´.´ | if | while
string      = ´"´ { character } ´"´
identifier  = letter
literal     = ´0´ | nonzero { digit }
nonzero     = ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
digit       = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
if          = ´[´ { statement } ´]´
while       = ´(´ { statement } ´)´
letter      = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ |
              ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ |
              ´x´ | ´y´ | ´w´ | ´z´ | ´A´ | ´B´ | ´C´ | ´D´ | ´E´ | ´F´ | ´G´ |
              ´H´ | ´I´ | ´J´ | ´K´ | ´L´ | ´M´ | ´N´ | ´O´ | ´P´ | ´Q´ | ´R´ |
              ´S´ | ´T´ | ´U´ | ´V´ | ´X´ | ´Y´ | ´W´ | ´Z´
punctuation = ´.´ | ´,´ | ´;´ | ´:´ | ´?´ | ´!´
special     = ´<´ | ´_´ | ´@´ | ´#´ | ´$´ | ´%´ | ´^´ | ´&´ | ´(´ | ´)´ | ´-´ |
              ´+´ | ´=´ | ´'´ | ´/´ | ´\´ | ´[´ | ´]´ | ´{´ | ´}´ | ´|´
blank       = ´ ´
character   = letter | digit | punctuation | special | blank
 */

class SyntaxAnalyzer(private var source: String) {

  private val it = new LexicalAnalyzer(source).iterator
  private var current: Lexeme = null
  
  // returns the current lexeme
  private def getLexeme(): Lexeme = {
    if (current == null) {
      current = it.next
    }
    //    println(current)
    current
  }

  // advances the input one lexeme
  private def nextLexeme() = {
    current = it.next
  }

  // TODOd: finish the recursive descent parser: need to finish parseStatement method
  // parses the program, returning its corresponding parse tree
  def parse() = {
    parseMouse()
  } // End parse method

  // 1.) mouse = { statement } '$$'
  private def parseMouse(): Tree =
  {
    // create a parse tree with non-terminal value "mouse"
    val tree = new Tree("mouse")
   
    while (getLexeme().getToken() != Token.EO_PRG) {
      if (getLexeme().getToken() == Token.EOF) {
        throw new Error("No '$$' found! Invalid Mouse file!")
      } // End if
      tree.add(parseStatement())
    } // End while
    
    val lexeme = getLexeme()
    val subTree = new Tree(lexeme.getLabel())
    tree.add(subTree)
    tree
  } // End parseMouse method

  /* 2.) statement =  ́? ́ |  ́! ́ | string | identifier |  ́= ́ | literal |
                      ́+ ́ |  ́- ́ |  ́* ́ |  ́/ ́ |  ́\ ́ |  ́^ ́ |  ́. ́ |
                      if | while
  */
  private def parseStatement(): Tree = {
    val lexeme = getLexeme()
    val tree = new Tree("statement")

    if (lexeme.getToken == Token.IDENTIFIER) {
      val subTree = new Tree("identifier")
      subTree.setAttribute("value", lexeme.getLabel())
      tree.add(subTree)
      nextLexeme()
    } // End if
        
    else if (lexeme.getToken == Token.LITERAL) {
      val subTree = new Tree("literal")
      subTree.setAttribute("value", lexeme.getLabel())
      tree.add(subTree)
      nextLexeme()
    } // End else if
        
    else if (lexeme.getToken == Token.STRING) 
      tree.add(parseString())

    else if (lexeme.getToken == Token.OPEN_BRACKET) 
      tree.add(parseIf())

    else if (lexeme.getToken == Token.OPEN_PAR) 
      tree.add(parseWhile())
      
   else if (lexeme.getToken == Token.CLOSE_BRACKET) 
      throw new Error("Syntax Analyzer error: Opening '[' expected!")
        
    else if (lexeme.getToken == Token.CLOSE_PAR) 
      throw new Error("Syntax Analyzer error: Opening '(' expected!")
      
    else {
      val subTree = new Tree(lexeme.getLabel())
      tree.add(subTree)
      nextLexeme()
    } // End else
      
    // Return tree
    tree
  } // End parseStatement


  // 3.) string =  ́" ́ { character }  ́" ́
  private def parseString(): Tree = {
    val lexeme = getLexeme()
    val tree = new Tree("string")
    tree.setAttribute("value", lexeme.getLabel())
    nextLexeme() 
    // Return tree
    tree
  } // End parseString
  
  // if = ´[´ { statement } ´]´
  private def parseIf(): Tree = {
    var lexeme = getLexeme()
    val tree = new Tree("if")
    while (getLexeme().getToken() != Token.CLOSE_BRACKET) {
      val subTree = new Tree(lexeme.getLabel())
      nextLexeme()
      tree.add(subTree)
      while (getLexeme().getToken() != Token.CLOSE_BRACKET) {
        if (getLexeme().getToken() == Token.EO_PRG) {
          throw new Error("Syntax Analyzer error: Closing ']' expected!")
        } // End if
        tree.add(parseStatement())
      } // End inner while
    } // End outer while
    
    lexeme = getLexeme()
    if (lexeme.getToken() == Token.CLOSE_BRACKET) {
      val subTree = new Tree(lexeme.getLabel())
      tree.add(subTree)
      nextLexeme()
    } // End if
    else
        throw new Error("Syntax Analyzer error: Closing ']' expected!")
    
    // Return tree
    tree
  } // End parseIf

  // while = ´(´ { statement } ´)´
  private def parseWhile(): Tree = {
    var lexeme = getLexeme()
    val tree = new Tree("while")
    while (getLexeme().getToken() != Token.CLOSE_PAR) {
      val subTree = new Tree(lexeme.getLabel())
      nextLexeme()
      tree.add(subTree)
      while (getLexeme().getToken() != Token.CLOSE_PAR) {
        if (getLexeme().getToken() == Token.EO_PRG) {
          throw new Error("Syntax Analyzer error: Closing ')' expected!")
        } // End if
        tree.add(parseStatement())
      } // End inner while
    } // End outer while
    
    lexeme = getLexeme()
    if (lexeme.getToken() == Token.CLOSE_PAR) {
      val subTree = new Tree(lexeme.getLabel())
      tree.add(subTree)
      nextLexeme()
    } // End if
    else
        throw new Error("Syntax analyzer error: Closing ')' expected!")
    
    // Return tree
    tree
  } // End parseWhile

  // ALL TERMINALS - NO METHOD REQUIRED
  // character = letter | digit | punctuation | special | blank

  // TERMINAL LINES
  //  nonzero     = ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
  //  letter      = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ |
  //  ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ |
  //  ´x´ | ´y´ | ´w´ | ´z´ | ´A´ | ´B´ | ´C´ | ´D´ | ´E´ | ´F´ | ´G´ |
  //  ´H´ | ´I´ | ´J´ | ´K´ | ´L´ | ´M´ | ´N´ | ´O´ | ´P´ | ´Q´ | ´R´ |
  //  ´S´ | ´T´ | ´U´ | ´V´ | ´X´ | ´Y´ | ´W´ | ´Z´
  //  punctuation = ´.´ | ´,´ | ´;´ | ´:´ | ´?´ | ´!´
  //  special     = ´<´ | ´_´ | ´@´ | ´#´ | ´$´ | ´%´ | ´^´ | ´&´ | ´(´ | ´)´ | ´-´ |
  //  ´+´ | ´=´ | ´'´ | ´/´ | ´\´ | ´[´ | ´]´ | ´{´ | ´}´ | ´|´
  //  blank       = ´ ´



} // End SyntaxAnalyzer class

object SyntaxAnalyzer {
  def main(args: Array[String]): Unit = {

    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    } // End if

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse()
    print(parseTree)
  } // End main
} // End object SyntaxAnalyzer
