module Parser where

import ParseType
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import GHC (parser)
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc=L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme ::Parser a->Parser a
lexeme=L.lexeme sc
symbol ::String->Parser String
symbol=L.symbol sc

charH=Parser.lexeme . char
stringH=Parser.lexeme . string

endOFLINE=charH ';'

term :: Parser Term
term=try $ (fmap ExprTerm (expr<*some endOFLINE))
        <|>(fmap DeclTerm (decl<*some endOFLINE)) 
        <|>some endOFLINE *> term


expr :: Parser EXPR
expr=try $ try ((charH '(' *> expr <* charH ')')
        <|>  do
                e<-expr'
                t<-try . optional $ do
                                    stringH "::"
                                    typE
                return (tagType e t)) 

expr' :: Parser EXPR
expr'=try $
        (try $ do
                e1<-(try . optional) expr''
                o<-op
                e2<-expr'
                case e1 of
                    Nothing->return (OpExpr o [e2] Nothing)
                    (Just e)->return (OpExpr o [e,e2] Nothing))  --Op
        <|>try (fmap (\x->AppExpr x Nothing) funApp) --FunApp
        <|>expr''


expr'' :: Parser EXPR
expr''
    =try (fmap (\x->ConstExpr x Nothing) constVal) --Const
    <|> try (fmap (\x->BlockExpr x Nothing) block) --Block
    <|> try (fmap (\x->VarExpr x Nothing) iden) --Id
    <|> try (fmap (\x->LambdaExpr x Nothing) lambda) --Lambda



decl :: Parser Decl
decl=try $ (varDecl <|> funDecl <|> typeDecl) -- <* newline

funDecl=try $ do
                stringH "func"
                id<-iden
                charH '('
                p<-param
                charH ')'
                many endOFLINE --deal with newline(self_defined)
                b<-block
                return (FunDecl id p b)

varDecl=try $ do
                stringH "var"
                id<-iden
                v<-try . optional $ charH '=' *> expr
                return (VarDecl id v Nothing)

typeDecl=try $ do
                id<-iden
                stringH "::"
                t<-typE
                return (TypeDecl id t)



typE :: Parser TYPE
typE=try $ singleType <|> arrow

singleType=try $ do 
                e<-expr
                return $ SingleType e

arrow=try $ do
                t<-singleType
                stringH"->"
                t2<-typE
                return $ ArrowType t t2

op :: Parser OP
op=try $ fmap (OP . Id ) $ stringH "+" <|> stringH "-" <|> stringH "*" <|> stringH "/" <|> stringH "="

block :: Parser Block
block=try $ do
                charH '{'
                e<-many term
                charH '}'
                return (Block e)

funApp :: Parser FunApp
funApp=try $ do
                i<-iden'
                charH '('
                p<-many expr
                charH ')'
                return $ FunApp i p



constVal :: Parser Val
constVal=try $ fmap ConstInt (Parser.lexeme L.decimal)

iden' :: Parser Id --No space followed
iden'=try $ do
                n<-alphaNumChar
                if elem n idStart then return () else fail " "
                n'<-idenh
                if elem (n:n') keywordList then fail " " else return $ Id (n:n')

idenh :: Parser String 
idenh=try $ do 
                n<-optional alphaNumChar
                case n of
                        Nothing->return []
                        Just x->if elem x illegalChar then return [] else do
                                                                xs<-idenh
                                                                return (x:xs)

iden :: Parser Id
iden=Parser.lexeme iden'

lambda :: Parser Lambda
lambda=try $ do
                charH '\\'
                p<-param
                stringH "->"
                b<-block
                return (Lambda p b)

param :: Parser [Id]
param=many iden

keywordList=["if","else","case","of","while","var","func"]
idStart=['A'..'Z']++['a'..'z']

illegalChar=['(',')','{','}',';']