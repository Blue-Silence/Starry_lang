import Parser
import qualified ParseType as P 
import Text.Megaparsec
import ASTConstrcut
import ASTType
import Text.Parsec (runParsecT)
test x=let (Right a)=runParser topMostParser " " x in a
t1=test "var a=1"
t2=test "var a=1 b :: int->int->int"
t3=test "var int=1 b :: int->int->int"

--instance Show ENV where
--    show  =foo 

-------------------------------------------------------------------------------------------------
-- A.test typeTransfer

{-testA x=let (Right a)=runParser typE " " x in a
symTab=[(P.Id "Int",[1,1]),(P.Id "Bool",[1,2]),(P.Id "a",[1,3]),(P.Id "f",[1,4])]
tA1=testA "(Forall a)=>Int->a->Bool"
tA2=testA "(Forall a)=>(((Forall a)=>a)->a->Bool)"
-}
-------------------------------------------------------------------------------------------



t4=test "var int=1 b :: int->int->int func b(x y) {  var b={var a=1:: int int(a x) int(x y)} :: int return y }"

{-a=FunDecl 
    [1] 
    [[1,1],[1,2]] 
    (Block 
        [BlockTerm (Term (ConExpr (ConReturn (VarExpr [1,3] Nothing) Nothing) Nothing [1,-1]))] 
        (ENV 
            [(Id "b",[1,1]),(Id "x",[1,2]),(Id "y",[1,3]),(Id "b",[1]),(Id "int",[2])] 
            [ValDecl [1,1] (BlockExpr (Block [BlockTerm (Term (AppExpr (FunApp [2] [VarExpr [1,1,1] Nothing,VarExpr [1,2] Nothing]) Nothing [1,1,-1])),BlockTerm (Term (AppExpr (FunApp [2] [VarExpr [1,2] Nothing,VarExpr [1,3] Nothing]) Nothing [1,1,-2]))] (ENV [(Id "a",[1,1,1]),(Id "b",[1,1]),(Id "x",[1,2]),(Id "y",[1,3]),(Id "b",[1]),(Id "int",[2])] [ValDecl [1,1,1] (ConstExpr (ConstInt 1) (Just (SingleType [2] (TypeENV [0] []))))] []) [1,1]) (Just (SingleType [2] (TypeENV [0] []))))] []) [1])
   -}

t5=test "var Type=0 data int :: Type {} b :: int->int->int func b(x y) {  var b={var a=1:: int int(a x) int(x y)} :: int return y }"