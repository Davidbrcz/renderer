module Main where


import Text.Parsec
import Control.Monad (void)

import Lib
data Dot = Undirected String Stmts | Directed String  deriving (Show)
data Stmt = NodeStmt String deriving (Show)
type Stmts = [Stmt]

semi = char ';'
open_curly = char '{'
close_curly = char '}'

identifier  = many1 letter


-- stmt_list = stmt `sepEndBy` semi
stmt_list = many stmt
stmt = do
  spaces
  nodeName <- identifier
  spaces >> optional semi
  return $ NodeStmt nodeName


graphDecl = do
  string "graph" >> spaces
  varName <- identifier
  stmts <-  between (spaces >> open_curly >> spaces) (spaces >> close_curly >> spaces) stmt_list
  return $ Undirected varName stmts

digraphDecl = do
  _ <- string "digraph" >> spaces
  varName <- identifier
  return $ Directed varName


topLevel2 = graphDecl <|> digraphDecl

main :: IO ()
main =
  case parse topLevel2 "" "graph foo{  a b ;   c  ; e}" of
    Right g -> print g
    Left err -> print err
