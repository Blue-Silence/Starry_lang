module TypeCheckingType where

import ParseType(Id(..),Val(..))

type TagC=Int

 
data ENV=ENV [(Id,Tag)] [Decl] [TypeDecl]
            deriving (Eq,Show)

data Decl=ValDecl Tag EXPR_C | ConstructOR Tag | FunDecl [Tag] Block
            deriving (Eq,Show)
data TypeDecl=TypeDecl Tag Type 
            deriving (Eq,Show)

data Block=Block [Block] ENV Tag| BlockTerm Term
            deriving (Eq,Show)

type Tag=[TagC]
type NameSpace=[(Id,Tag)]
data Type=SingleType Tag | ArrowType NameSpace Type Type| TypePlaceHolder Tag
            deriving (Eq,Show)
                                    
data EXPR_C 
    =ConstExpr Val (Maybe Type)
    |OpExpr OP [EXPR_C] (Maybe Type)
    |VarExpr Tag (Maybe Type)
    |ConExpr ConStruct (Maybe Type)
    |BlockExpr Block (Maybe Type)
    |LambdaExpr Lambda (Maybe Type)
    |AppExpr FunApp (Maybe Type)
        deriving (Eq,Show)

data Term=Term EXPR_C
    deriving (Eq,Show)
---------------------------------------------------------------------------------------------------------------
data FunApp=FunApp Tag [EXPR_C]
    deriving (Eq,Show)

newtype OP=OP Tag
    deriving (Eq,Show)

data Lambda=Lambda [Tag] Block
    deriving (Eq,Show)

data ConStruct
    =ConstrIf EXPR_C EXPR_C EXPR_C (Maybe Type)
    |ConstrWhile EXPR_C Block (Maybe Type)
    |ConstrCase EXPR_C [(Pattern,EXPR_C)] (Maybe Type)
    |ConReturn EXPR_C (Maybe Type)
    deriving (Eq,Show)

data Pattern=Pattern (Maybe Tag) EXPR_C
            deriving (Eq,Show)