module TypeCheck where

import ASTType

type ErrM=String

typeCheck :: [Module]->Either () ErrM
typeCheck ms = let  topTab=map (\(Module t e)->e) ms
                    re=map (\e->checkENV [mixENV topTab] e) topTab
                    in getRe re

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
                                                                ty'=checkBlock t paramType b
                                                                    in getRe [matchType (Left returnType) ty']

----------------------------------------------------------------------------------------------------------------------------



checkExpr :: [ENV]->EXPR_C ->Either Type ErrM
checkExpr es = undefined 

-----------------------------------------------------------------------------------------------------------------------------

matchType :: Either Type ErrM->Either Type ErrM->Either Type ErrM
--           Supposed type       True type
matchType x y = case getRe [x,y] of 
                    Right x->Right x 
                    Left _-> let    (Left st)=x 
                                    (Left tt)=y 
                                        in case matchTypeH [(0,[],[])] st tt of 
                                                Right _->Right ("Type mismatch. Expect:  "++(show st)++"  Got:  "++(show tt)++"\n")
                                                Left _->Left st

matchTypeH :: [(TagC,[(Tag,Type)],[(Tag,Type)])]->Type->Type->Either [(TagC,[(Tag,Type)],[(Tag,Type)])] ()
matchTypeH _ (SingleType _ _) (ArrowType _ _ _) = Right ()
matchTypeH ((tc,stv,ttv):as) t1@(ArrowType (TypeENV _ tye1) t11 t12) t2@(ArrowType (TypeENV _ tye2) t21 t22) = let  (tc',stv')=addPH tc stv tye1
                                                                                                                    (tc'',ttv')=addPH tc' ttv tye2
                                                                                                                        in case matching ((tc'',stv',ttv'):as) t11 t21 of
                                                                                                                            Left a->matchTypeH a t12 t22
                                                                                                                            Right _->Right ()
matchTypeH ((tc,stv,ttv):as) t1@(SingleType _ (TypeENV _ tye1)) t2@(SingleType _ (TypeENV _ tye2)) = let    (tc',stv')=addPH tc stv tye1
                                                                                                            (tc'',ttv')=addPH tc' ttv tye2
                                                                                                                in matching  ((tc'',stv',ttv'):as) t1 t2                                                                          
matchTypeH ((tc,stv,ttv):as) t1 t2@(SingleType _ (TypeENV _ tyenv)) = let    (tc',ttv')=addPH tc ttv tyenv
                                                                                                in matching  ((tc',stv,ttv'):as) t1 t2 
--matchTypeH a t1@(TypeExpr _) t2@(TypeExpr _) = matching a t1 t2                                                                                              
matchTypeH a b c = Right () --error ("matchTypeH function not matched. Param: "++(show a)++(show b)++(show c))

addPH tc ttv [] = (tc,ttv)
addPH tc ttv ((_,t):ts) = addPH (tc+1) ((t,TypePlaceHolder [tc]):ttv) ts

matching :: [(TagC,[(Tag,Type)],[(Tag,Type)])]->Type->Type->Either [(TagC,[(Tag,Type)],[(Tag,Type)])] ()
matching = undefined 

---------------------------------------------------------------------------------------------------------------------------
getFunType :: Tag->Type->[Tag]->([(Tag,Type)],Type)
getFunType lt a [] = ([],a)
getFunType lt (SingleType _ _) [_] = error "Number of param mismatch"
getFunType lt a@(ExprType _ _) _ = error ("Type structure not supported"++show a)
getFunType lt (ArrowType (TypeENV _ te) t1 t2) (tag1:tags)= let te'=fmap snd te
                                                                t1'=foldl (\x y->lift lt y x) t1 te'
                                                                t2'=foldl (\x y->lift lt y x) t2 te'
                                                                (xs,y)=getFunType lt t2' tags
                                                                    in ((tag1,t1'):xs,y)
getFunType _ (TypePlaceHolder _) _ = error " "

lift :: Tag -> Tag -> Type -> Type
lift lt t a@(SingleType tg env) = if tg==t then SingleType (lt++tg) env else a
lift lt t a@(ArrowType env t1 t2) = ArrowType env (lift lt t t1) (lift lt t t2) 
lift lt t a@(ExprType e env)=ExprType (liftExpr lt t e) env
 

liftExpr :: Tag -> Tag -> EXPR_C -> EXPR_C
liftExpr lt t (VarExpr tag mty) = let   mty'=fmap (lift lt t) mty
                                            in if tag==t then VarExpr (lt++t) mty' else VarExpr t mty'
liftExpr lt t (ConstExpr v mty) = ConstExpr v $ fmap (lift lt t) mty
liftExpr lt t (AppExpr (FunApp t1 es) mty tag) = let    mty'=fmap (lift lt t) mty
                                                        es'=map (liftExpr lt t) es 
                                                        t1'=if t1==t then lt++t1 else t1
                                                            in AppExpr (FunApp t1' es') mty' tag
liftExpr lt t (TypeExpr ty) = TypeExpr (lift lt t ty) 
liftExpr _ _ a = error "Type structure not support"

----------------------------------------------------------------------------


checkBlock :: [ENV]->[(Tag,Type)]->Block->Either Type ErrM
checkBlock t _ (BlockTerm (Term e)) = checkExpr t e
checkBlock t inject (Block bs (ENV symTab decls tds) tag) = let tds'=(map (\(t,ty)->TypeDecl t ty) inject)++tds 
                                                                t'=(ENV symTab decls tds'):t  
                                                                re1=map (checkBlock t' []) bs 
                                                                re2=map (checkDecl t) decls
                                                                returnVal=foldl splitBlock [] bs
                                                                returnType=foldl1 matchType (map (checkExpr t') returnVal)
                                                                    in case getRe [getRe re1,getRe re2,getRe [returnType]] of 
                                                                            Left _->returnType
                                                                            Right x->Right x

splitBlock :: [EXPR_C]->Block->[EXPR_C]
splitBlock re (BlockTerm (Term (ConExpr (ConReturn e _) _ _))) = e:re
splitBlock re _ = re

-------------------------------------------------------------------------------------------------------------
getType :: [ENV]->Tag->Either Type ErrM
getType tab t = case getType' (reverse tab) t t of
                    Just x->Left x 
                    Nothing->Right ("Type not found for : "++(show t))

getType' [] _ _=Nothing
getType' ((ENV _ _ tds):_) (_:_:[]) t'=lookup t' $ map (\(TypeDecl tag ty)->(tag,ty)) tds
getType' (_:tabs) (_:ts) t'=getType' tabs ts t'
getType' _ [] _=Nothing 


getRe :: [Either a1 [a2]] -> Either () [a2]
getRe [] = Left ()
getRe ((Left _):xs) = getRe xs 
getRe ((Right x):xs) = case getRe xs of 
                        (Left _)->Right x
                        (Right xs')->Right (x++xs')