module TypeCheck where

import ASTType

type ErrM=String

typeCheck :: [Module]->Either () ErrM
typeCheck ms = let  topTab=map (\(Module t e)->e) ms
                    re=map (\e->checkENV [mixENV topTab] e) topTab
                    in getRe re

getRe [] = Left ()
getRe ((Left _):xs) = getRe xs 
getRe ((Right x):xs) = case getRe xs of 
                        (Left _)->Right x
                        (Right xs')->Right (x++xs')

checkENV :: [ENV]->ENV->Either () ErrM
checkENV t (ENV _ ds _) = getRe $ map (checkDecl t) ds  

mixENV []=ENV [] [] []
mixENV ((ENV s d td):es)=let (ENV s' d' td')= mixENV es in ENV (s++s') (d++d') (td++td')


checkDecl :: [ENV]->Decl->Either () ErrM
checkDecl _ (ConstructOR _) = Left ()
checkDecl t (ValDecl tag e) = let   ty=checkExpr t e
                                    ty'=getType t tag
                                        in case (matchType ty' ty) of 
                                                    Left _ ->Left ()
                                                    (Right x)->Right x
checkDecl t (FunDecl tag ts b) = let    funType'=getType t tag
                                        in case funType' of 
                                            Right x->Right x
                                            Left funType->let   (paramType,returnType)=getFunType tag funType ts 
                                                                ty'=checkBlock paramType b
                                                                    in undefined





checkExpr :: [ENV]->EXPR_C ->Either Type ErrM
checkExpr = undefined 



matchType :: Either Type ErrM->Either Type ErrM->Either Type ErrM
matchType = undefined 

------------------------------------------------------------------------------------------------
getFunType :: Tag->Type->[Tag]->([(Tag,Type)],Type)
getFunType lt a [] = ([],a)
getFunType lt (SingleType _ _) [_] = error "Number of param mismatch"
getFunType lt a@(TypeExpr _) _ = error ("Type structure not supported"++show a)
getFunType lt (ArrowType (TypeENV _ te) t1 t2) (tag1:tags)= let te'=fmap snd te
                                                                t1'=foldl (\x y->lift lt y x) t1 te'
                                                                t2'=foldl (\x y->lift lt y x) t2 te'
                                                                (xs,y)=getFunType lt t2' tags
                                                                    in ((tag1,t1'):xs,y)
getFunType _ (TypePlaceHolder _) _ = error " "

lift lt t a@(SingleType tg env) = if tg==t then SingleType (lt++tg) env else a
lift lt t a@(ArrowType env t1 t2) = ArrowType env (lift lt t t1) (lift lt t t2) 
lift lt t a@(TypeExpr fun)=TypeExpr $ liftFunApp lt t fun

liftFunApp lt t (FunApp f ts) = let ts'=fmap (liftExpr lt t) ts
                                    in if f==t then FunApp (lt++f) ts' else FunApp f ts' 
liftExpr lt t (VarExpr tag mty) = let   mty'=fmap (lift lt t) mty
                                            in if tag==t then VarExpr (lt++t) mty' else VarExpr t mty'
liftExpr lt t (OpExpr op es mty tag) = let  mty'=fmap (lift lt t) mty
                                            es'=fmap (liftExpr lt t) es
                                                in (OpExpr op es' mty' tag)
liftExpr lt t (ConstExpr v mty) = ConstExpr v $ fmap (lift lt t) mty
liftExpr lt t (AppExpr funapp mty tag) = AppExpr (liftFunApp lt t funapp) (fmap (lift lt t) mty) tag
liftExpr _ _ _ = error "Type structure not support"

----------------------------------------------------------------------------


checkBlock :: [(Tag,Type)]->Block->Either Type ErrM
checkBlock = undefined 


getType :: [ENV]->Tag->Either Type ErrM
getType tab t = case getType' (reverse tab) t t of
                    Just x->Left x 
                    Nothing->Right ("Type not found for : "++(show t))

getType' [] _ _=Nothing
getType' ((ENV _ _ tds):_) (_:_:[]) t'=lookup t' $ map (\(TypeDecl tag ty)->(tag,ty)) tds
getType' (_:tabs) (_:ts) t'=getType' tabs ts t'
getType' _ [] _=Nothing 
