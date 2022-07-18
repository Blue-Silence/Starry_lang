module TypeChecking where

import Data.List
import TypeCheckingType
import qualified ParseType as P

pre :: [P.Decl]->P.Block
pre x = P.Block $ fmap P.DeclTerm x

scopeConstruct :: Tag->P.Block->Block
scopeConstruct tag (P.Block lt)= let (def,term)=splitDef ([],[]) lt in undefined


splitDef t []=t
splitDef (xs,ys) (z:zs)=case z of 
                                (P.DeclTerm x)->splitDef (x:xs,ys) zs
                                (P.ExprTerm y)->splitDef (xs,y:ys) zs

getID :: P.Decl->[P.Id]
getID (P.FunDecl x _ _) = [x]
getID (P.VarDecl x _ _)=[x]
getID (P.DataStrDecl x _ decl)=x:(concat $ map getID decl)
getID (P.TypeDecl x _)=[x]

genSymTab :: Tag->[P.Decl]->[(P.Id,Tag)]
genSymTab tag lt=let lt'=map head . group . sort .concat $ map getID lt in zipWith (\i t->(i,tagGen t tag)) lt' tagCLt
