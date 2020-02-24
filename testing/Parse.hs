{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
module Parse (parseCode) where

import Control.Applicative (Alternative (..))
import Control.Monad
import Prelude hiding (last)

-- Note: We do not need to import Hoopl to build an AST.
import Ast
import Expr
import           Text.ParserCombinators.Parsec hiding ((<|>), many)
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-- I'm stealing this parser almost directly from Daan Leijen's Parsec guide.
lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef {reservedOpNames = ["+", "-", "*", "/", "=", "<"]})

-- Common lexers:
lexeme, parens, braces :: CharParser () a -> CharParser () a
lexeme = P.lexeme lexer
parens = P.parens lexer
braces = P.braces lexer

commaSep :: CharParser () a -> CharParser () [a]
commaSep   = P.commaSep   lexer

reserved :: String -> CharParser () ()
reserved = P.reserved lexer

char' :: Char -> GenParser Char st ()
char' c = () <$ char c

identifier :: CharParser () String
identifier = P.identifier lexer

natural :: CharParser () Integer
natural = P.natural    lexer

reservedOp :: String -> CharParser () ()
reservedOp = P.reservedOp lexer

whitespace :: CharParser () ()
whitespace = P.whiteSpace lexer

brackets :: CharParser () a -> CharParser () a
brackets = P.brackets lexer

-- Expressions:
expr :: Parser Expr
expr = buildExpressionParser table factor
    <?> "Expression"
  where
    table = [[op "*"  (Binop Mul) AssocLeft, op "/"  (Binop Div) AssocLeft],
             [op "+"  (Binop Add) AssocLeft, op "-"  (Binop Sub) AssocLeft],
             [op "="  (Binop Eq)  AssocLeft, op "/=" (Binop Ne)  AssocLeft,
              op ">"  (Binop Gt)  AssocLeft, op "<"  (Binop Lt)  AssocLeft,
              op ">=" (Binop Gte) AssocLeft, op "<=" (Binop Lte) AssocLeft]]
    op o f = Infix (f <$ reservedOp o <?> "operator")
    factor =   parens expr
           <|> lit
           <|> load
           <|> fetchVar
           <?> "simple Expression"

bool :: Parser Bool
bool =  True  <$ try (lexeme (string "True"))
    <|> False <$ try (lexeme (string "False"))

lit :: Parser Expr
lit =  Lit . Int  <$> natural
   <|> Lit . Bool <$> bool
   <|> Lit . Bool <$> bool
   <?> "lit"

loc :: Char -> Parser x -> Parser x
loc s addr = try (lexeme (char' s >> brackets addr))
          <?> "loc"

var :: Parser String
var  = identifier
    <?> "var"

mem :: Parser Expr -- address
mem  =  loc 'm' expr
    <?> "mem"

fetchVar, load :: Parser Expr
fetchVar = Var  <$> var
load     = Load <$> mem


labl :: Parser Lbl
labl = lexeme (identifier <* char' ':') <?> "label"

mid :: Parser Insn
mid =   try asst
    <|> store
    <?> "assignment or store"

asst :: Parser Insn
asst = Assign <$> lexeme var <* lexeme (char' '=') <*> expr <?> "asst"

store :: Parser Insn
store = Store <$> lexeme mem <* lexeme (char' '=') <*> expr <?> "store"

control :: Parser Control
control =  branch
       <|> cond
       <|> call
       <|> ret
       <?> "control-transfer"


goto :: Parser Lbl
goto = lexeme (reserved "goto") *> identifier <?> "goto"

branch :: Parser Control
branch = Branch <$> goto <?> "branch"

cond, call, ret :: Parser Control
cond =
    Cond <$ lexeme (reserved "if")   <*> expr
         <* lexeme (reserved "then") <*> goto
         <* lexeme (reserved "else") <*> goto
 <?> "cond"

call = Call <$> tuple var <* lexeme (char' '=') <*> identifier <*> tuple expr <*> goto <?> "call"

ret = Return <$ lexeme (reserved "ret") <*> tuple expr <?> "ret"

block :: Parser Block
block =
  do { f   <- lexeme labl
     ; ms  <- many $ try mid
     ; l   <- lexeme control
     ; pure Block { first = f, mids = ms, last = l }
     }
 <?> "Expected basic block; maybe you forgot a label following a control-transfer?"

tuple :: Parser a -> Parser [a]
tuple = parens . commaSep

proc :: Parser Proc
proc = do { whitespace
          ; f      <- identifier
          ; params <- tuple  var
          ; bdy    <- braces (some block) -- procedure must have at least one block
          ; pure Proc { name = f, args = params, body = bdy }
          }
    <?> "proc"

parseCode :: String -> String -> Either ParseError [Proc]
parseCode = parse (many proc)
