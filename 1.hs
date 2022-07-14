module A where
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import GHC (parser)
import Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

data Scheme
    =SchemeData
    |SchemeFile
    |SchemeHttps
    |SchemeHttp
    deriving (Eq, Show)


f1=fmap Just (string' "abc") :: Parser (Maybe String)


f2 :: Parser Scheme
f2=choice [ fmap (const SchemeData) (string "Data")
          ,fmap (const SchemeHttps) (string "Https")
          ,fmap (const SchemeHttp) (string "Http")
          ,fmap (const SchemeFile) (string "File")]


f3=do
    a<-f2 <?> "Valid scheme"
    char ':'
    eof
    return Nothing

g x y=x <* y


data Op
    =Plus
    |Minus
    |Mul 
    |Div 
    |Neg
    deriving (Show,Eq)

data Exp
    =OpD Op Exp Exp
    |OpS Op Exp
    |Val Int
    deriving (Show,Eq)


{-expLex :: Parser Exp 



expLex1=do
    char '('
    a-<expLex
    char ')'
    return a

expLex2=do
-}

expa=exp0<*eof
exp0 :: Parser Exp
exp0=A.lexeme (choice[
                exp1])

exp1 :: Parser Exp
exp1=try (choice[exp1b
                ,exp1c
                ,exp2])

--exp1b :: Parser Exp
exp1b=try . A.lexeme $ do
    a<-exp3
    char '+'
    b<-exp1
    return (OpD Plus a b)

exp1c=try . A.lexeme $ do
    a<-exp3
    char '-'
    b<-exp1
    return (OpD Minus a b)


exp2 :: Parser Exp
exp2=try (choice[exp2b
                ,exp3
                ])

exp2b=try . A.lexeme $ do
    a<-exp3
    char '*'
    b<-exp1
    return (OpD Mul a b)

exp2c=try . A.lexeme $ do
    a<-exp3
    char '/'
    b<-exp1
    return (OpD Div a b)


exp3 :: Parser Exp
exp3=try exp3a <|>exp3b
exp3a=try.A.lexeme $do
        a<-L.decimal
        return (Val a)
exp3b=try (A.lexeme (char '(' )*> exp0 <* char ')')





sc :: Parser ()
sc=L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
lexeme ::Parser a->Parser a
lexeme=L.lexeme sc
symbol ::String->Parser String
symbol=L.symbol sc