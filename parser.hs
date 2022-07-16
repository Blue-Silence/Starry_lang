module Parser(topMostParser) where

import ParseType
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

topMostParser :: Parser [Decl]
topMostParser=many decl


term :: Parser Term
term=try $ (fmap DeclTerm decl)
        <|>(fmap ExprTerm expr)
        <|>some space1 *> term


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
    <|> try (fmap (\x->ConExpr x Nothing) controlStr) --constrol struction



decl :: Parser Decl
decl=try $ (varDecl <|> funDecl <|> typeDecl <|> dataStrDecl) 

funDecl=try $ do
                stringH "func"
                id<-iden
                charH '('
                p<-param
                charH ')'
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

dataStrDecl=try $ do 
                        stringH "data"
                        (TypeDecl id t)<-typeDecl
                        charH '{'
                        ts<-many typeDecl
                        charH '}'
                        return $ DataStrDecl id t ts



typE :: Parser TYPE
typE=try $ do
                c<-(try . optional) $ try $ do
                                                charH '('
                                                c1<-typeConstraint
                                                cs<-many $ try (charH ',' *>typeConstraint)
                                                charH ')'
                                                stringH "=>"
                                                return $ c1:cs
                t<-arrow <|> singleType
                case t of
                        SingleType e _->return $ SingleType e (concat c)
                        ArrowType e1 e2 _->return $ ArrowType e1 e2 (concat c) 

singleType=try $ do
                e<-expr
                return $ SingleType e []

arrow=try $ do
                t<-singleType
                stringH"->"
                t2<-typE
                return $ ArrowType t t2 []

typeConstraint=try $ do 
                        f<-iden
                        xs<-param
                        return $ TypeConstraint f xs

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

controlStr :: Parser ConStruct
controlStr=try $ conWhile <|> conIf <|> conCase <|> conReturn 

conWhile=try $ do 
                stringH "while"
                t<-expr
                b<-block 
                return $ ConstrWhile t b Nothing
conIf=try $ do
                stringH "if"
                t<-expr
                stringH "then"
                v1<-expr 
                stringH "else"
                v2<-expr 
                return $ ConstrIf t v1 v2 Nothing

conCase=try $ do
                stringH "case"
                t<-expr 
                stringH "of"
                cs<-many $ do 
                                p<-pattern 
                                stringH "->"
                                e<-expr
                                return (p,e)
                return $ ConstrCase t cs Nothing

conReturn=try $ do
                stringH "return"
                fmap (\x->ConReturn x Nothing) expr

pattern=try $ do 
                a<-optional (try $ iden'<* char '@')
                e<-expr
                return $ Pattern a e 


param :: Parser [Id]
param=many iden

keywordList=["if","else","case","of","while","var","func","data","return"]
idStart=['A'..'Z']++['a'..'z']

illegalChar=['(',')','{','}',';','@',',']







sc :: Parser ()
sc=L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme ::Parser a->Parser a
lexeme=L.lexeme sc
symbol ::String->Parser String
symbol=L.symbol sc

charH=Parser.lexeme . char
stringH=Parser.lexeme . string