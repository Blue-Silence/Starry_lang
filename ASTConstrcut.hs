module ASTConstrcut where

import Data.List
import TypeCheckingType
import qualified ParseType as P

pre :: [P.Decl]->P.Block
pre x = P.Block $ fmap P.DeclTerm x

scopeConstruct :: ENV->Tag->P.Block->Block
scopeConstruct preENV tag (P.Block lt)= let (def,term)=splitDef ([],[]) lt
                                            symTab=genSymTab tag def
                                            typeTab=genTypeTab symTab preENV def
                                            --env=ENV symTab
                                                in undefined


splitDef t []=t
splitDef (xs,ys) (z:zs)=case z of 
                                (P.DeclTerm x)->splitDef (x:xs,ys) zs
                                (P.ExprTerm y)->splitDef (xs,y:ys) zs
-------------------------------------------------------------------------------------------------------------------------
getID :: P.Decl->[P.Id]
getID (P.FunDecl x _ _) = [x]
getID (P.VarDecl x _ _)=[x]
getID (P.DataStrDecl x _ decl)=x:(concat $ map getID decl)
getID (P.TypeDecl x _)=[x]

genSymTab :: Tag->[P.Decl]->SymTab
genSymTab tag lt=let lt'=map head . group . sort .concat $ map getID lt in zipWith (\i t->(i,tagGen t tag)) lt' tagCLt

----------------------------------------------------------------------------------------------------------------------
genTypeTab :: SymTab->ENV->[P.Decl]->[TypeDecl]
genTypeTab = undefined 

genType :: SymTab->ENV->P.Decl->[TypeDecl]
genType _ _ (P.FunDecl _ _ _) = []
genType symTab preENV (P.VarDecl v _ t)=case t of 
                            Nothing->[]
                            Just ty->[TypeDecl (findTag symTab v) (typeTransfer symTab preENV EmptyTypeENV [0] ty)]
genType symTab preENV (P.DataStrDecl x t decl)=(TypeDecl (findTag symTab x) (typeTransfer symTab preENV EmptyTypeENV [0] t)):
                                                    (concat $ map (genType symTab preENV) decl)
genType symTab preENV (P.TypeDecl x ty)=[TypeDecl (findTag symTab x) (typeTransfer symTab preENV EmptyTypeENV [0] ty)]

typeTransfer :: SymTab->ENV->TypeENV->Tag->P.TYPE->Type
typeTransfer symTab preENV preTypeENV tag a@(P.SingleType e tyc) = let  tyNamesp=zipWith (\x y->(y,tag++[x])) tagCLt (getTypeVar tyc)
                                                                        symTab'=getSymTab' preTypeENV symTab tyNamesp
                                                                        typeENV=TypeENV tag tyNamesp preTypeENV
                                                                            in case (exprTransfer symTab' preENV e) of
                                                                                AppExpr funApp _->TypeExpr funApp
                                                                                VarExpr t _->SingleType t typeENV
                                                                                _->error $ "Illegal type structure" ++ show a
typeTransfer symTab preENV preTypeENV tag a@(P.ArrowType t1 t2 tyc) = let   tyNamesp=zipWith (\x y->(y,tag++[x])) tagCLt (getTypeVar tyc)
                                                                            symTab'=getSymTab' preTypeENV symTab tyNamesp
                                                                            typeENV=TypeENV tag tyNamesp preTypeENV
                                                                            t1'=typeTransfer symTab' preENV typeENV (tag++[1]) t1
                                                                            t2'=typeTransfer symTab' preENV typeENV (tag++[2]) t2 
                                                                                in ArrowType typeENV t1' t2'


findTag :: SymTab->P.Id->Tag
findTag ((iD,t):s) i=if iD==i then t else findTag s i 
findTag [] i=error ("Undefined varible"++(show i))

exprTransfer :: SymTab->ENV->P.EXPR->EXPR_C
exprTransfer = undefined 

getSymTab' :: TypeENV->SymTab->SymTab->SymTab
getSymTab' EmptyTypeENV x y=x++y
getSymTab' (TypeENV _ z t) x y= z++x++y++(getSymTab' t [] [])

getTypeVar :: [P.TypeConstraint]->[P.Id]
getTypeVar []= []
getTypeVar ((P.TypeConstraint (P.Id "Forall") [i]):xs)=i:getTypeVar xs -- Forall
getTypeVar x=error $ "Unsupport structure" ++ show x

