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
testA x=let (Right a)=runParser typE " " x in a
symTab=[(P.Id "Int",[1,1]),(P.Id "Bool",[1,2]),(P.Id "a",[1,3]),(P.Id "f",[1,4])]
tA1=testA "(Forall a)=>Int->a->Bool"
tA2=testA "(Forall a)=>(((Forall a)=>a)->a->Bool)"

-------------------------------------------------------------------------------------------



t4=test "var int=1 b :: int->int->int func b(x y) { return y }"