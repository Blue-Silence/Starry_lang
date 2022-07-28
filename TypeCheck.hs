module TypeCheck where

import ASTType
import ParseType(Val(..))
import Tools

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

tagToType t = SingleType t emptyTypeENV
tagToENV x env = (ENV [] [] []):env

checkExpr :: [ENV]->EXPR_C ->Either Type ErrM
checkExpr es (ConstExpr v mty) = let    ty=case v of
                                            ConstStr _->tagToType [1,2]
                                            ConstInt _->tagToType [1,3]
                                            ConstChar _->tagToType [1,4]
                                            ConstBool _->tagToType [1,5]
                                    in case mty of
                                            Nothing->Left ty
                                            Just ty'->if ty'==ty then Left ty else Right ("Can't match type. Expected: "++(show ty')++"  Actual: "++(show ty)++"\n")
checkExpr envs (OpExpr (OP o) es mty t) = case (getType envs o) of
                                            Left ft->let t=funappType (tagToENV t envs) ft es
                                                            in matchMaybeType mty t
                                            a->a
checkExpr envs (VarExpr t mty) = matchMaybeType mty (getType envs t)
checkExpr envs (BlockExpr b mty) = matchMaybeType mty (checkBlock envs [] b)
checkExpr envs (AppExpr (FunApp fe es) mty tag) = let   envs'=tagToENV tag envs
                                                        in case checkExpr envs' fe of
                                                                (Right x)->Right x
                                                                (Left ft)->funappType envs' ft es
checkExpr _ (TypeExpr _) = Left $ tagToType [1,1]
checkExpr envs (LambdaExpr (Lambda sym b@(Block _ _ tag)) mty) = case mty of
                                                Nothing->Right "Error:Lambda need type. \n"
                                                Just ft->let    (paramTypes,reT)=getFunType tag ft (fmap snd sym)
                                                                    in case matchType (Left reT) (checkBlock envs paramTypes b) of
                                                                            Right x->Right x
                                                                            Left _->Left ft
checkExpr envs a@(LambdaExpr (Lambda sym (BlockTerm (Term e))) mty) = Right ("Illegal lambda: "++show a++"\n")
checkExpr envs (ConExpr con mty tag) = let  envs'=tagToENV tag envs
                                                in matchMaybeType mty $ checkConS envs' con




funappType :: [ENV]->Type->[EXPR_C]->Either Type ErrM
funappType envs ft es = let es'=concatEither $ map (checkExpr envs) es
                                in case es' of
                                    Right x->Right x
                                    Left es''->let  st=foldr (\x acc->ArrowType emptyTypeENV x acc) UnknownRe es''
                                                    getReType [] a=Left a
                                                    getReType (_:xs) (ArrowType _ _ t)=getReType xs t
                                                    getReType _ _=Right "Param number too many \n"
                                                        in case matchType (Left st) (Left ft) of
                                                            Left t->getReType es t
                                                            a->a

checkConS :: [ENV]->ConStruct->Either Type ErrM
checkConS envs (ConstrIf eb e1 e2 mty)
    |isBool envs eb = matchMaybeType mty ty
    |otherwise = Right ("Expect Bool expr in if-structure \n") 
        where   ty=getGeneralType $ map (checkExpr envs) [e1,e2]
checkConS envs (ConstrWhile eb b mty)
    |isBool envs eb = matchMaybeType mty $ checkBlock envs [] b 
    |otherwise = Right ("Expect Bool expr in while-structure \n") 
checkConS envs (ConReturn e mty) = matchMaybeType mty $ checkExpr envs e
checkConS envs (ConstrCase ecase pes mty) = let caseT=checkExpr envs ecase
                                                et=getGeneralType $ map (checkPattern envs) pes
                                                in matchMaybeType mty et

checkPattern :: [ENV]->(Pattern,EXPR_C)->Either Type ErrM
checkPattern = undefined




getGeneralType :: [Either Type ErrM] -> Either Type ErrM
getGeneralType lt = let f a (Left b)=Left b
                        f a _=a 
                            in foldl1 f $ map (foldl1 matchType) (permutation lt)



isBool env e = case matchType (Left (SingleType [1,5] emptyTypeENV)) (checkExpr env e) of 
                    Left _->True 
                    _->False 

matchMaybeType (Just t1) t2 = matchType (Left t1) t2
matchMaybeType Nothing t2 = t2


-----------------------------------------------------------------------------------------------------------------------------

matchType :: Either Type ErrM->Either Type ErrM->Either Type ErrM
--           Supposed type       True type
matchType x y = case getRe [x,y] of
                    Right x->Right x
                    Left _-> let    (Left st)=x
                                    (Left tt)=y
                                    failMsg=Right ("Type mismatch. Expect:  "++(show st)++"  Got:  "++(show tt)++"\n")
                                        in case matchTypeH [(0,[],[])] st tt of
                                                Right _->failMsg
                                                Left (unknownT,as)->case sequence $ map (`concatType` as) unknownT of
                                                                            Just uTs->Left $ setUnknownType uTs st
                                                                            Nothing->failMsg       




matchTypeH :: [(TagC,[(Tag,Type)],[(Tag,Type)])]->Type->Type->Either ([Type],[(TagC,[(Tag,Type)],[(Tag,Type)])]) ()
matchTypeH _ (SingleType _ _) (ArrowType _ _ _) = Right ()
matchTypeH as UnknownRe tt = Left ([tt],as)
matchTypeH ((tc,stv,ttv):as) t1@(ArrowType (TypeENV _ tye1) t11 t12) t2@(ArrowType (TypeENV _ tye2) t21 t22)
    = let   (tc',stv')=addPH tc stv tye1
            (tc'',ttv')=addPH tc' ttv tye2
                in case matchTypeH ((tc''+1,[],[]):(tc'',stv',ttv'):as) t11 t21 of
                            Left (tts,(a:as))->addLt tts $matchTypeH as t12 t22
                            Right _->Right ()
matchTypeH a@((tc,stv,ttv):as) t1@(SingleType tag1 (TypeENV _ tye1)) t2@(SingleType tag2 (TypeENV _ tye2))
    |isTemp tag1 && isTemp tag2 = addELt $ setTmpVarType a' tag2 (getTmpVarTypeL a' tag1)
    |isTemp tag1 = Right ()
    |isTemp tag2 = addELt $ setTmpVarType a' tag2 (getTmpVarTypeL a' tag1)
    |otherwise = if tag1 == tag2 then Left ([],a) else Right ()
        where   (tc',stv')=addPH tc stv tye1
                (tc'',ttv')=addPH tc' ttv tye2
                a'=(tc'',stv',ttv'):as

matchTypeH ((tc,stv,ttv):as) t1 t2@(SingleType tag2 (TypeENV _ tyenv)) = let    (tc',ttv')=addPH tc ttv tyenv
                                                                                a'=((tc',stv,ttv'):as)
                                                                                    in addELt $ setTmpVarType a' tag2 t1
matchTypeH ((tc,stv,ttv):as) t1@(ExprType e1 (TypeENV _ tye1)) t2@(ExprType e2 (TypeENV _ tye2)) = let  (tc',stv')=addPH tc stv tye1
                                                                                                        (tc'',ttv')=addPH tc' ttv tye2
                                                                                                        a'=((tc''+1,[],[]):(tc'',stv',ttv'):as)
                                                                                                                in case matchTypeExpr a' e1 e2 of
                                                                                                                        Left (_:as)->addELt $ Left as
                                                                                                                        _->Right()
matchTypeH a b c = Right () --error ("matchTypeH function not matched. Param: "++(show a)++(show b)++(show c))

addLt x (Left (xs,ys)) = Left (x++xs,ys)
addLt _ a = a
addELt (Left x) = (Left ([],x))
addELt (Right x) = Right x

addN x = case x of
            Left n->Left (Nothing,n)
            Right n->Right n
delN x = case x of
            Left (_,x)->Left x
            Right x->Right x

addPH tc ttv [] = (tc,ttv)
addPH tc ttv ((_,t):ts) = addPH (tc+1) ((t,TypePlaceHolder [tc]):ttv) ts

isTemp x =  case x of
                (0:_)->True
                _->False

setTmpVarType :: [(TagC,[(Tag,Type)],[(Tag,Type)])]->Tag->Type->Either [(TagC,[(Tag,Type)],[(Tag,Type)])] ()
setTmpVarType a tag tarTy = let ty=getTmpVarTypeR a tag in
                                case ty of
                                    (TypePlaceHolder _)->Left $ map (\(a,b,c)->(a,b,map (\(t,ty')->(t,replaceTy ty tarTy ty')) c)) a
                                    _->delN $ matchTypeH a ty tarTy

getTmpVarTypeL :: [(TagC,[(Tag,Type)],[(Tag,Type)])]->Tag->Type
getTmpVarTypeL x = getTmpVarTypeH (concat (map (\(_,a,_)->a) x))
getTmpVarTypeR x = getTmpVarTypeH (concat (map (\(_,_,a)->a) x))
getTmpVarTypeH x t = let (Just s)=lookup t x in s
getTmpVarTypeR' x t = lookup t (concat (map (\(_,_,a)->a) x))

matchTypeExpr :: [(TagC,[(Tag,Type)],[(Tag,Type)])]->EXPR_C->EXPR_C->Either [(TagC,[(Tag,Type)],[(Tag,Type)])] ()
matchTypeExpr a (AppExpr (FunApp f1 es1) _ _) (AppExpr (FunApp f2 es2) _ _)
    |length es1 /= length es2 = Right ()
    |otherwise = foldl matchTypeEH (Left a) ((f1,f2):(zip es1 es2))
        where matchTypeEH a (x,y) = case a of
                                    (Left a')->matchTypeExpr a' x y
                                    _->Right ()
matchTypeExpr a (ConstExpr v1 _) (ConstExpr v2 _) = if v1==v2 then Left a else Right ()
matchTypeExpr a e1 (VarExpr v2 _) = if isTemp v2 then setTmpVarType a v2 (ExprType e1 (TypeENV [] [])) else Right ()
matchTypeExpr a (TypeExpr t1) (TypeExpr t2) = delN $ matchTypeH a t1 t2
matchTypeExpr _ _ _ = Right()


replaceTy ty tarTy t
    |t==ty = tarTy
    |otherwise = t

setUnknownType (t:_) UnknownRe = t 
setUnknownType (t:ts) (ArrowType env UnknownRe t2) = ArrowType env t $ setUnknownType ts t2 
setUnknownType ts (ArrowType env t1 t2) = ArrowType env t1 $ setUnknownType ts t2



concatType t@(SingleType tag e) as = if isTemp tag
                                        then
                                            case getTmpVarTypeR' as tag of
                                                                Just (TypePlaceHolder _)->Nothing
                                                                Just a->Just a
                                                                Nothing->Just t
                                            else Just t
concatType (ArrowType e t1 t2) as = do
                                        t1'<-concatType t1 as
                                        t2'<-concatType t2 as
                                        return $ ArrowType e t1' t2'
concatType (ExprType e env) as = do
                                    e'<-(concatTypeExpr e as)
                                    return $ ExprType e' env
concatType _ _ =Nothing

concatTypeExpr t@(ConstExpr _ _) _ = Just t
concatTypeExpr (TypeExpr e) as = fmap TypeExpr $ concatType e as
concatTypeExpr t@(VarExpr tag mty) as = if isTemp tag
                                        then
                                            case getTmpVarTypeR as tag of
                                                                TypePlaceHolder _->Just t
                                                                a->Just $ TypeExpr a
                                            else Just t
concatTypeExpr t@(AppExpr (FunApp e es) mty tag) as = do
                                                        e'<-concatTypeExpr e as
                                                        es'<-sequence $ fmap (\x->concatTypeExpr x as) es
                                                        return $ AppExpr (FunApp e' es') mty tag
concatTypeExpr _ _ = Nothing


---------------------------------------------------------------------------------------------------------------------------

getFunType :: Tag->Type->[Tag]->([(Tag,Type)],Type)
getFunType lt a [] = ([],a)
getFunType lt (SingleType _ _) (_:_) = error "Number of param mismatch"
getFunType lt a@(ExprType _ _) _ = error ("Type structure not supported"++show a)
getFunType lt (ArrowType (TypeENV _ te) t1 t2) (tag1:tags)= let te'=fmap snd te
                                                                t1'=foldl (\x y->lift lt y x) t1 te'
                                                                t2'=foldl (\x y->lift lt y x) t2 te'
                                                                (xs,y)=getFunType lt t2' tags
                                                                    in ((tag1,t1'):xs,y)
getFunType _ (TypePlaceHolder _) _ = error " "
getFunType _ UnknownRe  _ = error " "

lift :: Tag -> Tag -> Type -> Type
lift lt t a@(SingleType tg env) = if tg==t then SingleType (lt++tg) env else a
lift lt t a@(ArrowType env t1 t2) = ArrowType env (lift lt t t1) (lift lt t t2)
lift lt t a@(ExprType e env)=ExprType (liftExpr lt t e) env
lift _ _ a = a


liftExpr :: Tag -> Tag -> EXPR_C -> EXPR_C
liftExpr lt t (VarExpr tag mty) = let   mty'=fmap (lift lt t) mty
                                            in if tag==t then VarExpr (lt++t) mty' else VarExpr t mty'
liftExpr lt t (ConstExpr v mty) = ConstExpr v $ fmap (lift lt t) mty
liftExpr lt t (AppExpr (FunApp t1 es) mty tag) = let    mty'=fmap (lift lt t) mty
                                                        es'=map (liftExpr lt t) es
                                                        t1'=liftExpr lt t t1
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

concatEither t = let    f x acc =case acc of
                                Left as->case x of
                                            Left a->Left (a:as)
                                            Right x->Right x
                                Right e->case x of
                                            Left _->Right e
                                            Right x->Right (x++e)
                in foldr f (Left []) t
