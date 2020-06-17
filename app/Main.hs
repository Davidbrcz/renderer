module Main where


import System.IO
import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec.Char -- for letter
-- import Text.Parsec.Char
import Text.Parsec


 -- import Text.Parsec.Prim
 -- import Text.Parsec.Combinator


import Lib
type Id = String
data Dot = Undirected Id  Stmts
         | Directed Id  Stmts
         deriving (Show)

data Stmt = NodeStmt Id
            -- Attributes
          deriving (Show)
type Stmts = [Stmt]

data Attribute = Attribute Id Id deriving (Show)
type Attributes = [Attribute]

dotDef :: P.LanguageDef st
dotDef = P.LanguageDef
  { P.commentStart    = "/*"
  , P.commentEnd      = "*/"
  , P.commentLine     = "//"
  , P.nestedComments  = True
  , P.identStart      = letter
  , P.identLetter     = alphaNum
  , P.reservedNames   = ["node", "edge", "graph", "digraph", "subgraph", "strict" ]
  , P.caseSensitive   = True
  }



lexer = P.makeTokenParser dotDef

brackets    = P.brackets lexer
braces      = P.braces lexer

identifier  = P.identifier lexer
reserved    = P.reserved lexer

semi = P.semi lexer
-- comma = char ','
-- open_curly = char '{'
-- close_curly = char '}'
-- open_bracket = char '['
-- close_bracket = char ']'
-- identifier  = many1 letter

-- -- edge_stmt

-- -- -> Attribute
-- attribute = do
--   spaces
--   id1 <- identifier
--   spaces
--   char '='
--   id2 <- identifier
--   spaces
--   optional (semi <|> comma)
--   return $ Attribute id1 id2

-- -- -> [Attribute]
-- a_list = many attribute

-- -- -> [Attribute]
-- bracked_alist = between (spaces >> open_bracket >> spaces) (spaces >> close_bracket >> spaces) (option [] a_list)


-- attributes =
--   do
--     nestedAttributes <- many1 bracked_alist -- [[Attribute]]
--     return $ concat nestedAttributes


nodeStmt = do
  nodeName <- identifier
  -- attr <- option [] attributes
  return $ NodeStmt nodeName -- attr

stmt = do
  x <- nodeStmt
  spaces
  optional semi
  return x

-- stmt_list = stmt `sepEndBy` semi
stmt_list = many stmt
graphDecl = do
  reserved "graph"
  varName <- option "" identifier
  stms <- braces stmt_list
  return $ Undirected varName stms

digraphDecl = do
  reserved "digraph"
  varName <- option "" identifier
  stms <- braces stmt_list
  return $ Directed varName stms

topLevel3 = do
  spaces
  graphDecl <|> digraphDecl

main :: IO ()
main =
  case parse topLevel3 "" "digraph foo{  a b ;   c  ; e}" of
    Right g -> print g
    Left err -> print err
