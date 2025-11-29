{-# LANGUAGE NoOverloadedStrings #-}

module Main where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as M
import System.IO

-- ================================
-- TOKENS
-- ================================
data Token
    = NumberTok Double
    | IdentTok String
    | OpTok String
    | TAnd
    | TOr
    | TNot
    | TAssign
    | TLParen
    | TRParen
    | TSemicol
    | TEOF
    deriving (Show, Eq)


tokenize :: String -> [Token]
tokenize [] = [TEOF]
tokenize (c:cs)
    | c `elem` " \t" = tokenize cs
    | isDigit c =
        let (num, rest) = span (\x -> isDigit x || x == '.') (c:cs)
        in NumberTok (read num) : tokenize rest
    | isAlpha c =
        let (idstr, rest) = span isAlphaNum (c:cs)
        in case idstr of
            "and" -> TAnd : tokenize rest
            "or"  -> TOr  : tokenize rest
            "not" -> TNot : tokenize rest
            _     -> IdentTok idstr : tokenize rest
tokenize ('=':'=':cs) = OpTok "==" : tokenize cs
tokenize ('!':'=':cs) = OpTok "!=" : tokenize cs
tokenize ('>':'=':cs) = OpTok ">=" : tokenize cs
tokenize ('<':'=':cs) = OpTok "<=" : tokenize cs
tokenize (c:cs)
    | c `elem` "+-*/%><" = OpTok [c] : tokenize cs
    | c == '=' = TAssign : tokenize cs
    | c == '(' = TLParen : tokenize cs
    | c == ')' = TRParen : tokenize cs
    | c == ';' = TSemicol : tokenize cs
    | otherwise = error ("Unexpected char: " ++ [c])


-- ================================
-- AST
-- ================================
data AST
    = NumLit Double
    | Var String
    | Assign String AST
    | Unary String AST
    | BinOp String AST AST
    deriving (Show, Eq)

-- ================================
-- PARSER
-- ================================
newtype ParserState = PS { ts :: [Token] }

peek :: ParserState -> Token
peek (PS (t:_)) = t
peek _ = TEOF

advance :: ParserState -> (Token, ParserState)
advance (PS (t:rest)) = (t, PS rest)
advance s = (TEOF, s)

type Parser a = ParserState -> (a, ParserState)

expect :: (Token -> Bool) -> Parser Token
expect cond (PS (t:rest))
    | cond t = (t, PS rest)
    | otherwise = error ("Unexpected token: " ++ show t)
expect _ _ = error "Unexpected EOF"

-- Grammar functions

parseProgram :: Parser [AST]
parseProgram st =
    let (stmt1, st1) = parseStmt st
    in case peek st1 of
        TSemicol ->
            let (_, st2) = advance st1
                (rest, st3) = parseProgram st2
            in (stmt1 : rest, st3)
        _ -> ([stmt1], st1)

parseStmt :: Parser AST
parseStmt st =
    case peek st of
        IdentTok _ ->
            case ts st of
                (IdentTok v : TAssign : _) ->
                    let (_, st1) = advance st
                        (_, st2) = advance st1
                        (expr, st3) = parseExpr st2
                    in (Assign v expr, st3)
                _ -> parseExpr st
        _ -> parseExpr st

parseExpr :: Parser AST
parseExpr = parseOr

parseOr :: Parser AST
parseOr st =
    let (left, st1) = parseAnd st
    in case peek st1 of
        TOr ->
            let (_, st2) = advance st1
                (right, st3) = parseAnd st2
            in (BinOp "or" left right, st3)
        _ -> (left, st1)

parseAnd :: Parser AST
parseAnd st =
    let (left, st1) = parseEquality st
    in case peek st1 of
        TAnd ->
            let (_, st2) = advance st1
                (right, st3) = parseEquality st2
            in (BinOp "and" left right, st3)
        _ -> (left, st1)

parseEquality :: Parser AST
parseEquality st =
    let (left, st1) = parseComparison st
    in case peek st1 of
        OpTok "==" ->
            let (_, st2) = advance st1
                (right, st3) = parseComparison st2
            in (BinOp "==" left right, st3)
        OpTok "!=" ->
            let (_, st2) = advance st1
                (right, st3) = parseComparison st2
            in (BinOp "!=" left right, st3)
        _ -> (left, st1)

parseComparison :: Parser AST
parseComparison st =
    let (left, st1) = parseTerm st
    in case peek st1 of
        OpTok o | o `elem` [">","<",">=","<="] ->
            let (_, st2) = advance st1
                (right, st3) = parseTerm st2
            in (BinOp o left right, st3)
        _ -> (left, st1)

parseTerm :: Parser AST
parseTerm st =
    let (left, st1) = parseFactor st
    in case peek st1 of
        OpTok o | o `elem` ["+","-"] ->
            let (_, st2) = advance st1
                (right, st3) = parseFactor st2
            in (BinOp o left right, st3)
        _ -> (left, st1)

parseFactor :: Parser AST
parseFactor st =
    let (left, st1) = parseUnary st
    in case peek st1 of
        OpTok o | o `elem` ["*","/","%"] ->
            let (_, st2) = advance st1
                (right, st3) = parseUnary st2
            in (BinOp o left right, st3)
        _ -> (left, st1)

parseUnary :: Parser AST
parseUnary st =
    case peek st of
        TNot ->
            let (_, st1) = advance st
                (expr, st2) = parseUnary st1
            in (Unary "not" expr, st2)
        _ -> parsePrimary st

parsePrimary :: Parser AST
parsePrimary st =
    case peek st of
        NumberTok n ->
            let (_, st1) = advance st
            in (NumLit n, st1)

        IdentTok v ->
            let (_, st1) = advance st
            in (Var v, st1)

        TLParen ->
            let (_, st1) = advance st
                (expr, st2) = parseExpr st1
            in case peek st2 of
                TRParen ->
                    let (_, st3) = advance st2
                    in (expr, st3)
                _ -> error "Missing )"

        t -> error ("Unexpected token: " ++ show t)

-- ================================
-- INTERPRETER
-- ================================
type Env = M.Map String Double

evalAST :: Env -> AST -> (Double, Env)
evalAST env (NumLit n) = (n, env)

evalAST env (Var v) =
    case M.lookup v env of
        Just val -> (val, env)
        Nothing -> error ("Undefined variable: " ++ v)

evalAST env (Assign v expr) =
    let (val, env2) = evalAST env expr
        env3 = M.insert v val env2
    in (val, env3)

evalAST env (Unary "not" expr) =
    let (v, env2) = evalAST env expr
        result = if v == 0 then 1 else 0
    in (result, env2)

evalAST env (BinOp op l r) =
    let (lv, env1) = evalAST env l
        (rv, env2) = evalAST env1 r
        result = case op of
            "+" -> lv + rv
            "-" -> lv - rv
            "*" -> lv * rv
            "/" -> lv / rv
            "%" -> fromIntegral (floor lv `mod` floor rv)
            "==" -> boolToNum (lv == rv)
            "!=" -> boolToNum (lv /= rv)
            "<"  -> boolToNum (lv < rv)
            ">"  -> boolToNum (lv > rv)
            "<=" -> boolToNum (lv <= rv)
            ">=" -> boolToNum (lv >= rv)
            "and" -> boolToNum ((lv /= 0) && (rv /= 0))
            "or"  -> boolToNum ((lv /= 0) || (rv /= 0))
            _ -> error ("Unknown operator: " ++ op)
    in (result, env2)

boolToNum :: Bool -> Double
boolToNum b = if b then 1 else 0

-- ================================
-- REPL
-- ================================
main :: IO ()
main = do
    putStrLn "=== Advanced Expression Interpreter (Haskell) ==="
    putStrLn "Type expressions or assignments. Use ';' to chain statements."
    putStrLn "Type 'exit' to quit.\n"
    loop M.empty
  where
    loop env = do
        putStr ">>> "
        hFlush stdout
        line <- getLine
        if line == "exit"
            then return ()
            else do
                let toks = tokenize line
                    (stmts, _) = parseProgram (PS toks)
                    (result, newEnv) = evalMany env stmts
                print result
                loop newEnv

    evalMany :: Env -> [AST] -> (Double, Env)
    evalMany env [] = (0, env)
    evalMany env [x] = evalAST env x
    evalMany env (x:xs) =
        let (_, env2) = evalAST env x
        in evalMany env2 xs
