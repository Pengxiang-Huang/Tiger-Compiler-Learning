type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

fun err(p1,p2) = ErrorMsg.error p1

val comment_level = ref 0 ;(* for nested comments *)

val stringstart = ref 0; (* record the yypos for started strings *)

val mystr = ref  ""        (* for strings *)

fun eof() = 
let 
  val pos = hd(!linePos) ;
  val unclosed = (!comment_level > 0)
in 
  if unclosed then (ErrorMsg.error pos "error: unclosed comment"; Tokens.EOF(pos,pos))
  else
  Tokens.EOF(pos,pos) 
end

fun getnum str = case Int.fromString str of
        SOME n => n
      | NONE => 0 (* should never happen if regex is correct *)

fun getchar str = case Char.fromString str of 
        SOME c => c
      | NONE => Char.chr 0 (* should never happen if regex is correct *)

fun getddd str = let
        val str1 = String.substring(str,1,3);
        val n = getnum str1;
        val c = Char.chr n;
        val s = Char.toString c
        in s end

fun getcontrol str = let
        val capital = String.substring(str,2,1);
        val char = getchar capital;
        val code = Char.ord char - 64;
        val resultchar = Char.chr code;
        val s = Char.toString resultchar
        in s end

%% 
%s INITIAL COMMENT STRING SLASHF SLASH;

letter = [a-zA-Z];
digit = [0-9];
id = {letter}({letter}|{digit}|_)*;
space = [\ \t];
quote = ["];
notQuote = [^"];
backslash = [\\];
notBackslash = [^\\];
control = [\^];
illegal_escape = [^nrt]; 
capital = [A-Z];
%%

<INITIAL> var  	   => (Tokens.VAR(yypos,yypos+ size yytext));
<INITIAL> while    => (Tokens.WHILE(yypos,yypos+ size yytext));
<INITIAL> for      => (Tokens.FOR(yypos,yypos+ size yytext));
<INITIAL> to       => (Tokens.TO(yypos,yypos+ size yytext));
<INITIAL> break    => (Tokens.BREAK(yypos,yypos+ size yytext));
<INITIAL> let      => (Tokens.LET(yypos,yypos+ size yytext));
<INITIAL> in       => (Tokens.IN(yypos,yypos+ size yytext));
<INITIAL> end      => (Tokens.END(yypos,yypos+ size yytext));
<INITIAL> function => (Tokens.FUNCTION(yypos,yypos+ size yytext));
<INITIAL> type     => (Tokens.TYPE(yypos,yypos+ size yytext));
<INITIAL> array    => (Tokens.ARRAY(yypos,yypos+ size yytext));
<INITIAL> if       => (Tokens.IF(yypos,yypos+ size yytext));
<INITIAL> then     => (Tokens.THEN(yypos,yypos+ size yytext));
<INITIAL> else     => (Tokens.ELSE(yypos,yypos+ size yytext));
<INITIAL> do       => (Tokens.DO(yypos,yypos+ size yytext));
<INITIAL> of       => (Tokens.OF(yypos,yypos+ size yytext));
<INITIAL> nil      => (Tokens.NIL(yypos,yypos+ size yytext));

<INITIAL> {digit}+ => (Tokens.INT(getnum yytext,yypos,yypos+ size yytext));
<INITIAL> {id}     => (Tokens.ID(yytext,yypos,yypos+ size yytext));

<INITIAL> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> \r	=> (continue());
<INITIAL> \t	=> (continue());
<INITIAL> \f	=> (continue());
<INITIAL> {backslash}{illegal_escape} => (ErrorMsg.error yypos ("illegal escape character " ^ yytext); continue());
<INITIAL>  {space}	=> (continue());

<INITIAL> "/*"	=> (YYBEGIN COMMENT; comment_level := !comment_level+1; continue());
<COMMENT> "/*"	=> (comment_level := !comment_level+1; continue());
<COMMENT> "*/"	=> (if !comment_level = 1 then (YYBEGIN INITIAL; comment_level := !comment_level-1) else (comment_level := !comment_level-1); continue());
<COMMENT> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT> .     => (continue());

<INITIAL>  {quote}  => (YYBEGIN STRING; mystr := "" ; stringstart := yypos ;continue());
<STRING>  \n => (ErrorMsg.error (!stringstart) "error: string begin with new line"; YYBEGIN INITIAL; lineNum := !lineNum+1; linePos := yypos :: !linePos; continue() );
<STRING>  {backslash}n   => ( mystr := !mystr ^ "\n"; continue());
<STRING>  {backslash}t  => ( mystr := !mystr ^ "\t"; continue());
<STRING>  {backslash}r  => ( mystr := !mystr ^ "\r"; continue());
<STRING>  {backslash}{backslash}  => ( mystr := !mystr ^ "\\"; continue());
<STRING>  {backslash}{quote}  => ( mystr := !mystr ^ "\""; continue());
<STRING>  {backslash}{digit}{digit}{digit}  => (let val asci_char = getddd yytext in mystr := !mystr ^ asci_char end; continue());
<STRING>  {backslash}{control}{capital} => (let val asci_char = getcontrol yytext in mystr := !mystr ^ asci_char end; continue());

<STRING>  {backslash}\n => (YYBEGIN SLASH; continue());
<STRING>  {backslash}{space} => (YYBEGIN SLASH; continue());
<STRING>  {backslash}\t => (YYBEGIN SLASH; continue());

<STRING>  {backslash}. => (ErrorMsg.error yypos ("illegal escape character " ^ yytext); continue());

<STRING>  {notQuote}  => (mystr := !mystr ^ yytext; continue());
<STRING>   {quote}  => (YYBEGIN INITIAL; Tokens.STRING(!mystr,!stringstart,yypos) );

<SLASH>  {notBackslash} => (continue());
<SLASH>   {backslash} => (YYBEGIN STRING; continue());

<STRING>  {backslash}f  => (YYBEGIN SLASHF; continue());
<SLASHF>   .    => (continue());
<SLASHF>   f{backslash} => (YYBEGIN STRING; continue());

<INITIAL> ","	  => (Tokens.COMMA(yypos,yypos+ size yytext));
<INITIAL> ":"   => (Tokens.COLON(yypos,yypos+ size yytext));
<INITIAL> ";"   => (Tokens.SEMICOLON(yypos,yypos+ size yytext));
<INITIAL> "("   => (Tokens.LPAREN(yypos,yypos+ size yytext));
<INITIAL> ")"   => (Tokens.RPAREN(yypos,yypos+ size yytext));
<INITIAL> "["   => (Tokens.LBRACK(yypos,yypos+ size yytext));
<INITIAL> "]"   => (Tokens.RBRACK(yypos,yypos+ size yytext));
<INITIAL> "{"   => (Tokens.LBRACE(yypos,yypos+ size yytext));
<INITIAL> "}"   => (Tokens.RBRACE(yypos,yypos+ size yytext));
<INITIAL> "."   => (Tokens.DOT(yypos,yypos+ size yytext));
<INITIAL> "+"   => (Tokens.PLUS(yypos,yypos+ size yytext));
<INITIAL> "-"   => (Tokens.MINUS(yypos,yypos+ size yytext));
<INITIAL> "*"   => (Tokens.TIMES(yypos,yypos+ size yytext));
<INITIAL> "/"   => (Tokens.DIVIDE(yypos,yypos+ size yytext));
<INITIAL> "="   => (Tokens.EQ(yypos,yypos+ size yytext));
<INITIAL> "<>"  => (Tokens.NEQ(yypos,yypos+ size yytext));
<INITIAL> "<"   => (Tokens.LT(yypos,yypos+ size yytext));
<INITIAL> "<="  => (Tokens.LE(yypos,yypos+ size yytext));
<INITIAL> ">"   => (Tokens.GT(yypos,yypos+ size yytext));
<INITIAL> ">="  => (Tokens.GE(yypos,yypos+ size yytext));
<INITIAL> "&"   => (Tokens.AND(yypos,yypos+ size yytext));
<INITIAL> "|"   => (Tokens.OR(yypos,yypos+ size yytext));
<INITIAL> ":="  => (Tokens.ASSIGN(yypos,yypos+ size yytext));

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());