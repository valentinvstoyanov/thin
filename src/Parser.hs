module Parser(parseExpr, parseLambdaExpr, stringifyExpr) where

import Syntax 

stringifyExpr :: Expr -> String 
stringifyExpr (Var v)   = varNameOf v
stringifyExpr (Lam v e) = "\\" ++ (varNameOf v) ++ "." ++ (stringifyExpr e)
stringifyExpr (App f e) = "(" ++ (stringifyExpr f) ++ (stringifyExpr e) ++ ")"

parseLambdaExpr :: String -> Expr
parseLambdaExpr [] = error "Can't parse lambda expression from empty string."
parseLambdaExpr s@(l:x:y:xs)
    | l == '\\' && isAlpha x = case y of 
        '.'  -> Lam (VarName [x]) (parseExpr xs)
        '\\' -> Lam (VarName [x]) (parseLambdaExpr (y:xs))
        ch   -> if isAlpha ch then Lam (VarName [x]) (parseLambdaExpr ("\\" ++ y : xs)) 
                else error $ "Failed to recognize the expression after the lambda parameter: " ++ [ch]
    | otherwise = error $ "Invalid lambda expression: " ++ s

parseExpr :: String -> Expr
parseExpr []      = error "Can't parse expression from empty string."
parseExpr s@[_] = parseVar s
parseExpr s@(x:xs)
    | x == '\\'             = parseLambdaExpr s 
    | x == '(' || isAlpha x = parseApp s
    | otherwise             = error $ "Failed to recognize the expression: " ++ s

parseVar :: String -> Expr 
parseVar []    = error "Can't parse variable from empty string."
parseVar v@[_] = Var $ VarName v

parseApp :: String -> Expr
parseApp [] = error "Can't parse empty app."
parseApp a  = let (t:ts) = tokenizeApp a in foldl App t ts 

tokenizeApp :: String -> [Expr]
tokenizeApp [] = []
tokenizeApp s@(x:xs) 
    | x == '('  = let (prefix, suffix) = matchParanthesesSpan s in parseExpr prefix : tokenizeApp suffix 
    | isAlpha x = parseExpr [x] : tokenizeApp xs  
    | otherwise = error $ "Can't tokenize app: " ++ s

isAlpha :: Char -> Bool
isAlpha ch = ch >= 'a' && ch <= 'z'

alphaSpan :: String -> (String, String) 
alphaSpan = span isAlpha 

matchParanthesesSpan :: String -> (String, String)
matchParanthesesSpan str@(x:xs) 
    | x == '('  = let (prefix, suffix) = matchParanthesesSpan' xs 1 "" in (init prefix, suffix)
    | otherwise = error $ "Cannot find matching parantheses on string that doesn't start with such: " ++ str
    where matchParanthesesSpan' :: String -> Int -> String -> (String, String)
          matchParanthesesSpan' [] 0 acc = (reverse acc, "")
          matchParanthesesSpan' [] _ _   = error "No matching parantheses found(too many opening)."
          matchParanthesesSpan' s@(p:ps) c acc 
              | c == 0    = (reverse acc, s)
              | c < 0     = error "No matching parantheses found(too many closing)."
              | p == ')'  = matchParanthesesSpan' ps (c - 1) (p : acc)
              | p == '('  = matchParanthesesSpan' ps (c + 1) (p : acc)
              | otherwise = matchParanthesesSpan' ps c (p : acc)
