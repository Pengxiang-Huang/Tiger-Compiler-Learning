structure Parse : sig val parse : string -> unit
                      val parseandprint: string -> string -> unit
                  end =
struct 
  structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
  structure Lex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
  structure TigerP = Join(structure ParserData = TigerLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)
  fun parse filename =
      let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
	  val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
	  fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	  val (absyn, _) = TigerP.parse(30,lexer,parseerror,())
      in TextIO.closeIn file;
	 TextIO.output(TextIO.stdErr, "\n");
	 PrintAbsyn.print (TextIO.stdErr,absyn);
	 TextIO.output(TextIO.stdErr, "\n")
      end handle LrParser.ParseError =>
              (TextIO.output(TextIO.stdErr,
                             "Parser cannot repair the program\n");
	       ())

  fun parseandprint infilename outfilename =
      let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := infilename)
	  val infile = TextIO.openIn infilename
	  fun get _ = TextIO.input infile
	  fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	  val (absyn, _) = TigerP.parse(30,lexer,parseerror,())
          val outfile = TextIO.openOut outfilename
       in TextIO.closeIn infile;
	   PrintAbsyn.print (outfile,absyn);
	   TextIO.closeOut outfile
      end handle LrParser.ParseError => 
              (TextIO.output(TextIO.stdErr,
                             "Parser cannot repair the program\n");
	       ())

end

