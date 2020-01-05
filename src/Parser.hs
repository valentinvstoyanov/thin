module Parser(parseExpr, parseLambdaExpr, stringifyExpr) where

import Syntax 

-- Function that converts given expression to beautiful-ish string.
stringifyExpr :: Expr -> String 
stringifyExpr (Var v)   = varNameOf v
stringifyExpr (Lam v e) = "\\" ++ (varNameOf v) ++ "." ++ (stringifyExpr e)
stringifyExpr (App f e) = "(" ++ (stringifyExpr f) ++ (stringifyExpr e) ++ ")"

-- Function which constructs lambda expression given properly formatted string.
-- e.g "\\yx.xy"
parseLambdaExpr :: String -> Expr
parseLambdaExpr [] = error "Can't parse lambda expression from empty string."
parseLambdaExpr s@(l:x:y:xs)
    | l == '\\' && isAlpha x = case y of 
        '.'  -> Lam (VarName [x]) (parseExpr xs)
        '\\' -> Lam (VarName [x]) (parseLambdaExpr (y:xs))
        ch   -> if isAlpha ch then Lam (VarName [x]) (parseLambdaExpr ("\\" ++ y : xs)) 
                else error $ "Failed to recognize the expression after the lambda parameter: " ++ [ch]
    | otherwise = error $ "Invalid lambda expression: " ++ s

-- Function that constructs expression given properly formatted string. 
-- e.g "x", "\\x.x", "(xy)"
parseExpr :: String -> Expr
parseExpr []      = error "Can't parse expression from empty string."
parseExpr s@[_] = parseVar s
parseExpr s@(x:xs)
    | x == '\\'             = parseLambdaExpr s 
    | x == '(' || isAlpha x = parseApp s
    | otherwise             = error $ "Failed to recognize the expression: " ++ s

-- Function that constructs variable expression given string with only one character.
-- e.g "x", "y"
parseVar :: String -> Expr 
parseVar []    = error "Can't parse variable from empty string."
parseVar v@[_] = Var $ VarName v

-- Function that constructs application given properly formatted string.
-- e.g "(xy)", "xyz", "(xy)z"
parseApp :: String -> Expr
parseApp [] = error "Can't parse empty app."
parseApp a  = let (t:ts) = tokenizeApp a in foldl App t ts 

-- Function that takes application as a properly formatted string and parses it into tokens.
-- e.g "xy" results into [(TVar (VarName "x")), (TVar (VarName "y"))] 
--     "xy" -> [x, y]
-- e.g "(xy)z" results int [(TArr (TVar (VarName "x")) (TVar (VarName "y"))), (TVar (VarName "z"))]
--     "(xy)z" -> [(xy), z]
tokenizeApp :: String -> [Expr]
tokenizeApp [] = []
tokenizeApp s@(x:xs) 
    | x == '('  = let (prefix, suffix) = matchParanthesesSpan s in parseExpr prefix : tokenizeApp suffix 
    | isAlpha x = parseExpr [x] : tokenizeApp xs  
    | otherwise = error $ "Can't tokenize app: " ++ s

-- Checks whether the given character is a-z.
isAlpha :: Char -> Bool
isAlpha ch = ch >= 'a' && ch <= 'z'

-- Function that takes string starting with '(', finds the corresponding ')'
-- and returns a pair of the string inbetween the parantheses and the string after ')'.
-- e.g "(xy)wz" results into ("xy", "wz") 
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
