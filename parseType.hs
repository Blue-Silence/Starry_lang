module ParseType where 

data Id=Id String
    deriving (Eq,Show)

data EXPR
    =ConstExpr Val (Maybe TYPE)
    |OpExpr OP [EXPR] (Maybe TYPE)
    |VarExpr Id (Maybe TYPE)
    -- |ConExpr ConStruct 
    |BlockExpr Block (Maybe TYPE)
    |LambdaExpr Lambda (Maybe TYPE)
    |DeclExpr Decl 
    |AppExpr FunApp (Maybe TYPE)
-- |TypeDeclExpr TypeDecl
    deriving (Eq,Show)

data Val
    =ConstStr String
    |ConstInt Int 
    |ConstChar Char 
    |ConstBool Bool
    deriving (Eq,Show)

data TYPE=SingleType Id [Id] | ArrowType TYPE TYPE
        deriving (Eq,Show)

data OP=OP Id
    deriving (Eq,Show)

data Block=Block [EXPR]
    deriving (Eq,Show)

data Lambda=Lambda [Id] Block
    deriving (Eq,Show)

data Decl=FunDecl Id [Id] Block | VarDecl Id (Maybe EXPR) (Maybe TYPE) | TypeDecl Id TYPE
    deriving (Eq,Show)

data FunApp=FunApp Id [EXPR]
    deriving (Eq,Show)

--data TypeDecl=TypeDecl Id TYPE

tagType :: EXPR->Maybe TYPE->EXPR
tagType e t=case e of
            ConstExpr x _ ->ConstExpr x t
            OpExpr x y _->OpExpr x y t 
            VarExpr x _->VarExpr x t
            BlockExpr x _->BlockExpr x t
            LambdaExpr x _->LambdaExpr x t
            AppExpr x _->AppExpr x t
            _->e
