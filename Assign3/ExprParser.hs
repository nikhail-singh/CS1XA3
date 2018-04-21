{-|
Module : ExprParser
Description : Contains functions that parse Strings into the expression dataType
Copyright : (c) Nikhail Singh @2018
License : WTFPL
Maintainer : singhn18@mcmaster.ca
Stability : experimental
Portability : POSIX.
-}
module ExprParser (parseExprD,parseExprF) where

import           ExprType
import           ExprDiff

import           Text.Parsec
import           Text.Parsec.String

-- | this function performs the parsing of doubles
parseExprD :: String -- ^ The string to be parsed 
            -> Expr Double -- The resulting Double Expression
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

constParserD :: Parser (Expr Double)-- ^ parser for constants
constParserD = do { a <- doubleDigits;
                   return $ Const (read a)}

-- | this function is a combination of parsers to parse doubles
exprD :: Parser (Expr Double)
exprD = let
        factor = (parens exprD) <|> miscOps (parens exprD) <|> constParserD <|> varParser
        terms = factor `chainl1` powParser
        power =  factor `chainl1` mulOp
    in power `chainl1` addOp

parseExprF :: String -- ^ The string to be parsed 
            -> Expr Float -- ^ the resulting Float Expression
parseExprF ss = case parse exprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

-- | this function is a combination of parsers to parse floats
exprF :: Parser (Expr Float)
exprF = let
        factor = (parens exprF) <|> miscOps (parens exprF) <|> constParserF <|> varParser
        terms = factor `chainl1` powParser
        power =  factor `chainl1` mulOp
    in power `chainl1` addOp

-- | parser for constants
constParserF :: Parser (Expr Float) 
constParserF = do { a <- doubleDigits;
                   return $ Const (read a)}


-- | parser for variables such as x or y
varParser :: Parser (Expr a)  
varParser = do { x <- many1 alphaNum;
                return (Var x)}

-- | parser for powers
powParser :: (Eq a,ExprEval a,Fractional a, Floating a) => Parser (Expr a -> Expr a -> Expr a)
powParser = do { symbol "^"; return (!^) }

-- | parser for multiplication and division 
mulOp :: (Eq a,ExprEval a,Fractional a, Floating a) => Parser (Expr a -> Expr a -> Expr a)
mulOp = do { symbol "*"; return (!*) }
    <|> do { symbol "/"; return (!/) }

-- | parser for addition and subtraction 
addOp :: (Eq a,ExprEval a,Fractional a, Floating a) => Parser (Expr a -> Expr a -> Expr a)
addOp = do { symbol "+"; return (!+) }
    <|> do { symbol "-"; return (!-) }

-- | parser for trig and e^x and Ln 
miscOps :: (Eq a,ExprEval a,Fractional a, Floating a) => Parser (Expr a) -> Parser (Expr a)
miscOps x = do { symbol "exp"; y <- x; return $ exp1 y}
        <|> do { symbol "sin"; y <- x; return $ sine y}
        <|> do { symbol "cos"; y <- x; return $ cosine y}
        <|> do { symbol "tan"; y <- x; return $ tangent y}
        <|> do { symbol "ln"; y <- x; return $ ln1 y}

-- | parser that removes parenthesis
parens :: Parser a -> Parser a 
parens p = do { char '(';
                cs <- p;
                char ')';
                return cs }

-- | parser for various strings such as "sin" or "ln"
symbol :: String -> Parser String 
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

-- | Parser that deals with  floating points
doubleDigits :: Parser String -- ^ parser that can handle either decimal numbers or integers
doubleDigits = do { ds <- try negDigits <|> digits ;
                    rs <- try decimalDigits <|> return "" ;
                    return $ ds ++ rs }

-- | Parser that deals with  floating points
decimalDigits :: Parser String 
decimalDigits = do { d <- char '.' ;
                     rm <- digits ;
                     return $ d:rm }

-- | parser that handles the '-' sign
negDigits :: Parser String 
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }
-- | parser that handles regular digits
digits :: Parser String 
digits = many1 digit