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
                            Just ty->[TypeDecl (findTag symTab v) (typeTransfer symTab preENV ty)]
genType symTab preENV (P.DataStrDecl x t decl)=(TypeDecl (findTag symTab x) (typeTransfer symTab preENV t)):
                                                    (concat $ map (genType symTab preENV) decl)
genType symTab preENV (P.TypeDecl x ty)=[TypeDecl (findTag symTab x) (typeTransfer symTab preENV ty)]

typeTransfer :: SymTab->ENV->TypeENV->P.TYPE->Type
typeTransfer symTab preENV TypeENV = let namesp=(getTypeVar tyc)
                                        in typeExpr symTab preENV namesp e
                                                            {-in case exprC of
                                                                AppExpr funApp _->TypeExpr funApp
                                                                VarExpr t _->SingleType t
                                                                _->error $ "Illegal type structure" ++ show a -}
typeTransfer symTab preENV a@(P.ArrowType t1 t2 tyc) = let  namesp=(getTypeVar tyc)
                                                                    t1'= typeTransfer symTab preENV namesp t1
                                                                    t2'= typeTransfer symTab preENV namesp t2
                                                            in ArrowType n t1' t2'

findTag :: SymTab->P.Id->Tag
findTag ((iD,t):s) i=if iD==i then t else findTag s i 
findTag [] i=error ("Undefined varible"++(show i))

exprTransfer :: SymTab->ENV->P.EXPR->EXPR_C
exprTransfer = undefined 

typeExpr :: SymTab->ENV->[[(P.Id,TagC)]]->P.EXPR->Type 
typeExpr = undefined 


--addTempVar :: [P.Id]->->SymTab 
--addTempVar = undefined 

getTypeVar :: [P.TypeConstraint]->[P.Id]
getTypeVar []= []
getTypeVar ((P.TypeConstraint (P.Id "Forall") [i]):xs)=i:getTypeVar xs -- Forall
getTypeVar x=error $ "Unsupport structure" ++ show x


--foo :: ENV->P.Decl->Decl 
--foo (P.FunDecl f xs block)=FunDecl (findTag x f)
