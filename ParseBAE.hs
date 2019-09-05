module ParseBAE where

  import System.IO
  import Control.Monad
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token

  type Name = String

  data Stmt = If Stmt Stmt Stmt 
            | Let Name Stmt Stmt
            | ABUnary ABUn Stmt
            | ABBinary ABBin Stmt Stmt
            | ABRel RBin Stmt Stmt
            | Var Name
            | Num Integer
            | BoolCt Bool deriving(Show)

  data ABUn = Not | Succ | Pred deriving(Show)

  data ABBin = And | Or | Add | Times deriving(Show)

  data RBin = GrT | LowT | Equ deriving(Show)

  languageDef = 
    emptyDef { Token.commentStart   = "/*"
             , Token.commentEnd     = "*/"
             , Token.commentLine    = "//"
             , Token.identStart     = letter
             , Token.identLetter    = alphaNum
             , Token.reservedNames  = ["if",
                                       "then",
                                       "else",
                                       "true",
                                       "false",
                                       "not",
                                       "and",
                                       "or",
                                       "suc",
                                       "pred",
                                       "let",
                                       "in",
                                       "end"]
             , Token.reservedOpNames = ["+","*",">","<","=",":=","and","or","not"]}

  lexer = Token.makeTokenParser languageDef

  identifier = Token.identifier lexer
  reserved = Token.reserved lexer
  reservedOp = Token.reservedOp lexer
  parens = Token.parens lexer
  integer = Token.integer lexer
  whiteSpace = Token.whiteSpace lexer

  baeParser :: Parser Stmt
  baeParser = whiteSpace >> parens statement

  statement :: Parser Stmt
  statement = ifStmt
            <|> letStmt
            <|> buildExpressionParser abOperators (parens statement
            <|> liftM Num integer
            <|> liftM Var identifier
            <|> (reserved "true" >> return (BoolCt True))
            <|> (reserved "false" >> return (BoolCt False)))

  ifStmt :: Parser Stmt
  ifStmt =
    do reserved "if"
       cond <- statement
       reserved "then"
       stmt1 <- statement
       reserved "else"
       stmt2 <- statement
       return $ If cond stmt1 stmt2

  letStmt :: Parser Stmt
  letStmt =
    do reserved "let"
       var <- identifier
       reserved ":="
       stmt1 <- statement
       reserved "in"
       stmt2 <- statement
       reserved "end"
       return $ Let var stmt1 stmt2

  abOperators = [[Prefix (reservedOp "not" >> return (ABUnary Not))]
                ,[Prefix (reservedOp "suc" >> return (ABUnary Succ)),
                  Prefix (reservedOp "pred" >> return (ABUnary Pred))]
                ,[Infix (reservedOp "and" >> return (ABBinary And)) AssocLeft,
                  Infix (reservedOp "or" >> return (ABBinary Or)) AssocLeft]
                ,[Infix (reservedOp "*" >> return (ABBinary Times)) AssocLeft]
                ,[Infix (reservedOp "+" >> return (ABBinary Add)) AssocLeft]
                ,[Infix (reservedOp ">" >> return (ABRel GrT)) AssocLeft]
                ,[Infix (reservedOp "<" >> return (ABRel LowT)) AssocLeft]
                ,[Infix (reservedOp "=" >> return (ABRel Equ)) AssocLeft]]

  parseString :: String -> Stmt
  parseString str =
    case parse baeParser "" str of
      Left e -> error $ show e
      Right r -> r

  parseFile :: String -> IO Stmt
  parseFile file =
    do program <- readFile file
       case parse baeParser "" program of 
        Left e -> print e >> fail "Parsing error."
        Right r -> return r