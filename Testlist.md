LEXER
- **OK** 5 lexes to INT(5)
- **OK** "String" lexes to STRING("String")
- **OK** x lexes to IDENTIFIER("x")
- **OK** 1.2 lexes to FLOAT(1.2)
- **OK** 1.2f lexes to FLOAT(1.2)
- **OK** plus lexes to OP("+")
- **OK** 1+2 lexes to INT(1),OP("+"),INT(2)
- **OK** true lexes to BOOL(true)
- **OK** true+1 lexes to BOOL(true),OP("+"),INT(1)
- **OK** true"2+1"1 lexes to BOOL(true),STRING("2+1"),INT(1)
- **OK** (space) true lexes to BOOL(true)
- **OK** true1 lexes to identifier
- **OK** "max" gives STRING("max")
- **OK** String gives SIMPLE_TYPE(String)
- **OK** null gives NULL()
- **OK** let fun null gives LET(),FUN(),NULL()
- **OK** while if else (){}=:,; gives correct tokens
- **OK** =>match case class .new gives correct tokens
- **OK** // lexes to OP("//")
- **OK** "# comment\n1" lexes to INT(1)
- **OK** /* multiline \n \n comment */1 lexes to List()

PARSER
- **OK** 5 parses to IntLit(5)
- **OK** (5) parses to IntLit(5)
- **OK** 1 + 2 parses to BinOpExp(IntLit(1), PlusBinOp(), IntLit(2))
- **OK** 1.2*2 parses to BinOpExp(FloatLit(1.2), PlusBinOp(), IntLit(2))
- **OK** 1.2//2 parses to BinOpExp(FloatLit(1.2), FracBinOp(), IntLit(2))

INTERPRETER
- **OK** 1.2//2 evaluates to FracVal(FloatVal(1.2), IntVal(2))
- **OK** \_1//12345678*12345678\_ evaluates to 1
- **OK** \_1//12345678\_*12345678" does not eval to 1
- **OK** \_1.2//4//0.25\_ evaluates to 1.2
- **OK** {let x = 5;let y = 10; x * y} evaluates to 50
- **OK** {let x = 5;let y = 10; x * y}*2 evaluates to 100
