module ASTType(module ASTType,Id(..),Val(..)) where

import ParseType(Id(..),Val(..))

type TagC=Integer 
tagCLt=[1..] -- 0 is preserved as type varible
             -- Term block start at 1. Type block start at 0. 

tagCLtAny=[-1,-2..] -- negative number used to tag anonymous scope

data Module=Module Tag ENV
        deriving (Ord,Eq,Show)
 
type SymTab=[(Id,Tag)]

data ENV=ENV SymTab [Decl] [TypeDecl]
            deriving (Ord,Eq,Show)

data Decl=ValDecl Tag EXPR_C | ConstructOR Tag Int | FunDecl Tag [Tag] Block | Param Tag | Foreign Tag
            deriving (Ord,Eq,Show)
data TypeDecl=TypeDecl Tag Type 
            deriving (Ord,Eq,Show)

data Block=Block [Block] ENV Tag| BlockTerm Term
            deriving (Ord,Eq,Show)

type Tag=[TagC]
type NameSpace=[Id]
data Type=SingleType Tag TypeENV | ArrowType TypeENV Type Type| TypePlaceHolder Tag | ExprType EXPR_C TypeENV | UnknownRe
            deriving (Ord,Eq,Show)
                                    
data TypeENV=TypeENV Tag SymTab
            deriving (Ord,Eq,Show)

emptyTypeENV = TypeENV [] []

data EXPR_C 
    =ConstExpr Val (Maybe Type)
    |OpExpr OP [EXPR_C] (Maybe Type) Tag
    |VarExpr Tag (Maybe Type)
    |ConExpr ConStruct (Maybe Type) Tag
    |BlockExpr Block (Maybe Type)
    |LambdaExpr Lambda (Maybe Type)
    |AppExpr FunApp (Maybe Type) Tag
    |TypeExpr Type
        deriving (Ord,Eq,Show)

data Term=Term EXPR_C
    deriving (Ord,Eq,Show)
---------------------------------------------------------------------------------------------------------------
data FunApp=FunApp EXPR_C [EXPR_C]
    deriving (Ord,Eq,Show)

newtype OP=OP Tag
    deriving (Ord,Eq,Show)

data Lambda=Lambda SymTab Block
    deriving (Ord,Eq,Show)

data ConStruct
    =ConstrIf EXPR_C EXPR_C EXPR_C (Maybe Type)
    |ConstrWhile EXPR_C Block (Maybe Type)
    |ConstrCase EXPR_C [(Pattern,EXPR_C)] (Maybe Type)
    |ConReturn EXPR_C (Maybe Type)
    deriving (Ord,Eq,Show)

data Pattern=Pattern (Maybe Tag) EXPR_C
            deriving (Ord,Eq,Show)

---------------------------------------------------------------------------------------------

tagGen :: TagC->Tag->Tag 
tagGen x xs=xs++[x]