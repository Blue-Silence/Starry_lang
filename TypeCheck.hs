module TypeCheck where

import ASTType

type ErrM=String

typeCheck :: [Module]->Either () ErrM
typeCheck ms = let  topTab=map (\(Module t e)->(t,e)) ms
                    re=map (\(_,e)->checkENV [topTab] e) topTab
                    in getRe re

getRe [] = Left ()
getRe ((Left _):xs) = getRe xs 
getRe ((Right x):xs) = case getRe xs of 
                        (Left _)->Right x
                        (Right xs')->Right (x++xs')

checkENV :: [[(Tag,ENV)]]->ENV->Either () ErrM
checkENV t (ENV _ ds _) = getRe $ map (checkDecl t) ds  

checkDecl :: [[(Tag,ENV)]]->Decl->Either () ErrM
checkDecl _ (ConstructOR _) = Left ()
checkDecl t (ValDecl tag e) = let   ty=checkExpr t e
                                    ty'=getType t tag
                                        in case (matchType ty' ty) of 
                                                    Left _ ->Left ()
                                                    (Right x)->Right x
checkDecl t (FunDecl tag ts b) = let    funType=getType t tag
                                        (paramType,returnType)=getFunType tag funType ts 
                                        ty'=checkBlock paramType b
                                            in undefined





checkExpr :: [[(Tag,ENV)]]->EXPR_C ->Either Type ErrM
checkExpr = undefined 

getType :: [[(Tag,ENV)]]->Tag->Either Type ErrM
getType = undefined 

matchType :: Either Type ErrM->Either Type ErrM->Either Type ErrM
matchType = undefined 

getFunType :: Tag->Either Type ErrM->[Tag]->([(Tag,Type)],Type)
getFunType = undefined

checkBlock :: [(Tag,Type)]->Block->Either Type ErrM
checkBlock = undefined 