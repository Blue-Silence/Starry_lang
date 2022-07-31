import ASTType
import TypeCheck
import ParseType(Id(..),Val(..))

---------------------------------------------------------------------------
-- testMatchType

tm11 = SingleType [1,1] emptyTypeENV
tm12 = SingleType [0,1] (TypeENV [0] [(Id "x",[0,1])])

tye1=(TypeENV [0,1] [(Id "x",[0,1,1])])
tye2=(TypeENV [0] [(Id "x",[0,1]),(Id "y",[0,2])])
tm21 = ArrowType tye2 (SingleType [0,1,1] tye1) tm11
tm21' = ArrowType tye2 (SingleType [0,1] tye1) tm11
tm22 = ArrowType tye2 tm11 tm11

tm30 = ArrowType tye1 (SingleType [0,1] emptyTypeENV) (SingleType [0,1,1] emptyTypeENV)

tm31 = ArrowType emptyTypeENV (ArrowType tye1 (SingleType [0,1,1] emptyTypeENV) (SingleType [1,1] emptyTypeENV)) (SingleType [1,1] emptyTypeENV )
tm32 = ArrowType tye2 tm30 (SingleType [0,1] emptyTypeENV)


tm41 = ArrowType tye2 (SingleType [0,1] emptyTypeENV) (SingleType [0,1] emptyTypeENV)
tm42 = ArrowType emptyTypeENV (SingleType [1,1] emptyTypeENV) UnknownRe 

-----------------------------------------------------------------------------
-- test getFunType

tg1 = tm21'
