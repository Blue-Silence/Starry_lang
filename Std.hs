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





genMod :: Tag->[FPair]->Module
genMod tgM lt = let (a,b,c)=foldl (\(a,b,c) d->((Id $ name d,tag d):a,(Foreign $ tag d):b,(TypeDecl (tag d) (tYPE d)):c)) ([],[],[]) lt
                    in Module tgM (ENV a b c)

genC :: [FPair]->String 
genC [] = ""
genC (d:ds)= case def d of 
                Just df->"(define " ++ (tagCGEN $ tag d) ++ df ++ "  )\n" ++ genC ds 
                Nothing->genC ds

