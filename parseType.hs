module ParseType where 

newtype Id=Id String
    deriving (Ord,Eq,Show)

data EXPR
    =ConstExpr Val (Maybe TYPE)
    |OpExpr OP [EXPR] (Maybe TYPE)
    |VarExpr Id (Maybe TYPE)
    |ConExpr ConStruct (Maybe TYPE)
    |BlockExpr Block (Maybe TYPE)
    |LambdaExpr Lambda (Maybe TYPE)
    |AppExpr FunApp (Maybe TYPE)
    |TypeExpr TYPE
    deriving (Ord,Eq,Show)


data Term 
    =DeclTerm Decl
    |ExprTerm EXPR
    deriving (Ord,Eq,Show)


data Val
    =ConstStr String
    |ConstInt Int 
    |ConstChar Char 
    |ConstBool Bool
    |ConstVoid 
    deriving (Ord,Eq,Show)

data TYPE=SingleType EXPR [TypeConstraint]| ArrowType TYPE TYPE [TypeConstraint]
        deriving (Ord,Eq,Show)

newtype OP=OP Id
    deriving (Ord,Eq,Show)

newtype Block=Block [Term]
    deriving (Ord,Eq,Show)

data Lambda=Lambda [Id] Block
    deriving (Ord,Eq,Show)

data Decl=FunDecl Id [Id] Block | VarDecl Id (Maybe EXPR) (Maybe TYPE) | TypeDecl Id TYPE | DataStrDecl Id TYPE [Decl]
    deriving (Ord,Eq,Show)

data FunApp=FunApp EXPR [EXPR]
    deriving (Ord,Eq,Show)

data ConStruct
    =ConstrIf EXPR EXPR EXPR (Maybe TYPE)
    |ConstrWhile EXPR Block (Maybe TYPE)
    |ConstrCase EXPR [(Pattern,EXPR)] (Maybe TYPE)
    |ConReturn EXPR (Maybe TYPE)
    deriving (Ord,Eq,Show)

data Pattern=Pattern (Maybe Id) EXPR
            deriving (Ord,Eq,Show)

data TypeConstraint=TypeConstraint Id [Id]
                deriving (Ord,Eq,Show)


tagType :: EXPR->Maybe TYPE->EXPR
tagType e t=case e of
            ConstExpr x _ ->ConstExpr x t
            OpExpr x y _->OpExpr x y t 
            VarExpr x _->VarExpr x t
            BlockExpr x _->BlockExpr x t
            LambdaExpr x _->LambdaExpr x t
            AppExpr x _->AppExpr x t
            _->e
