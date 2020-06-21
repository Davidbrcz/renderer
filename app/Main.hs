module Main where

import System.Environment
import System.IO
import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec.Char -- for letter
-- import Text.Parsec.Char
import Text.Parsec


import Lib
type Id = String
data Dot = Undirected Id  Stmts
         | Directed Id  Stmts
         deriving (Show)

data Stmt = NodeStmt Id Attributes
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
  , P.opStart         = oneOf "=-"
  , P.opLetter        = oneOf "->"
  }



lexer = P.makeTokenParser dotDef

brackets    = P.brackets lexer
braces      = P.braces lexer

identifier  = P.identifier lexer
reserved    = P.reserved lexer

semi = P.semi lexer
comma = P.comma lexer

reservedOp = P.reservedOp lexer

eq_op = reservedOp "="
undir_edge_op = reservedOp "--"
dir_edge_op = reservedOp "->"

-- -- edge_stmt

-- -- -> Attribute
attribute = do
  id1 <- identifier
  eq_op
  id2 <- identifier
  optional (semi <|> comma)
  return $ Attribute id1 id2

a_list = many attribute

bracked_alist =
  brackets $ option [] a_list

attributes =
  do
    nestedAttributes <- many1 bracked_alist -- [[Attribute]]
    return $ concat nestedAttributes


nodeStmt = do
  nodeName <- identifier
  attr <- option [] attributes
  return $ NodeStmt nodeName attr

stmt = do
  x <- nodeStmt
  optional semi
  return x

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
main = do
  (file:_) <- getArgs
  content <- readFile file
  case parse topLevel3 "" content of
    Right g -> print g
    Left err -> print err
