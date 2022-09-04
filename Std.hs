module Std where 

import ASTType
import Codegen(tagCGEN)

stdENV = ENV []

data FPair = FPair {
    name :: String 
    ,tag :: Tag
    ,tYPE :: Type
    ,def :: Maybe String
}
    deriving Show

dType=FPair {
    name="Type"
    ,tag=[1,1]
    ,tYPE=UnknownRe
    ,def=Nothing
}

dString=FPair {
    name="String"
    ,tag=[1,2]
    ,tYPE=SingleType [1,1] emptyTypeENV
    ,def=Nothing
}

dInt=FPair {
    name="Int"
    ,tag=[1,3]
    ,tYPE=SingleType [1,1] emptyTypeENV
    ,def=Nothing
}

dChar=FPair {
    name="Char"
    ,tag=[1,4]
    ,tYPE=SingleType [1,1] emptyTypeENV
    ,def=Nothing
}

dBool=FPair {
    name="Bool"
    ,tag=[1,5]
    ,tYPE=SingleType [1,1] emptyTypeENV 
    ,def=Nothing
}

dVoid=FPair {
    name="Void"
    ,tag=[1,6]
    ,tYPE=SingleType [1,1] emptyTypeENV 
    ,def=Nothing
}




dPlus=FPair{
    name="+"
    ,tag=[2,1]
    ,tYPE= ArrowType 
                emptyTypeENV 
                (SingleType [1,3] emptyTypeENV ) 
                (ArrowType 
                    emptyTypeENV 
                    (SingleType [1,3] emptyTypeENV)
                    (SingleType [1,3] emptyTypeENV ))
    ,def=Just "+"
}

dMinus=FPair{
    name="-"
    ,tag=[2,2]
    ,tYPE= ArrowType 
                emptyTypeENV 
                (SingleType [1,3] emptyTypeENV ) 
                (ArrowType 
                    emptyTypeENV 
                    (SingleType [1,3] emptyTypeENV)
                    (SingleType [1,3] emptyTypeENV ))
    ,def=Just "-"
}

dMul=FPair{
    name="*"
    ,tag=[2,3]
    ,tYPE= ArrowType 
                emptyTypeENV 
                (SingleType [1,3] emptyTypeENV ) 
                (ArrowType 
                    emptyTypeENV 
                    (SingleType [1,3] emptyTypeENV)
                    (SingleType [1,3] emptyTypeENV ))
    ,def=Just "*"
}

dDiv=FPair{
    name="/"
    ,tag=[2,4]
    ,tYPE= ArrowType 
                emptyTypeENV 
                (SingleType [1,3] emptyTypeENV ) 
                (ArrowType 
                    emptyTypeENV 
                    (SingleType [1,3] emptyTypeENV)
                    (SingleType [1,3] emptyTypeENV ))
    ,def=Just "/"
}

dHead=FPair{
    name="head"
    ,tag=[2,5]
    ,tYPE= ArrowType 
                emptyTypeENV 
                (SingleType [1,2] emptyTypeENV )
                (SingleType [1,4] emptyTypeENV ) 
    ,def=Just "car"
}

dTail=FPair{
    name="tail"
    ,tag=[2,4]
    ,tYPE= ArrowType 
                emptyTypeENV 
                (SingleType [1,2] emptyTypeENV )
                (SingleType [1,2] emptyTypeENV ) 
    ,def=Just "cdr"
}

dIsNull=FPair{
    name="isnull"
    ,tag=[2,4]
    ,tYPE= ArrowType 
                emptyTypeENV 
                (SingleType [1,2] emptyTypeENV )
                (SingleType [1,5] emptyTypeENV ) 
    ,def=Just "null?"
}

dPut=FPair{
    name="put"
    ,tag=[2,4]
    ,tYPE= ArrowType 
                emptyTypeENV 
                (SingleType [1,4] emptyTypeENV )
                (SingleType [1,6] emptyTypeENV ) 
    ,def=Just "display"
}


genMod :: Tag->[FPair]->Module
genMod tgM lt = let (a,b,c)=foldl (\(a,b,c) d->((Id $ name d,tag d):a,(Foreign $ tag d):b,(TypeDecl (tag d) (tYPE d)):c)) ([],[],[]) lt
                    in Module tgM (ENV a b c)

genC :: [FPair]->String 
genC [] = "\n\n"
genC (d:ds)= case def d of 
                Just df->"(define " ++ (tagCGEN $ tag d) ++ df ++ " )\n" ++ genC ds 
                Nothing->genC ds


stdM1 = genMod [1] [dType,dString,dInt,dChar,dBool,dVoid]
stdM2 = genMod [2] [dPlus,dMinus,dMul,dDiv,dHead,dTail,dIsNull,dPut]

std1 = genC [dType,dString,dInt,dChar,dBool,dVoid]
std2 = genC [dPlus,dMinus,dMul,dDiv,dHead,dTail,dIsNull,dPut]
