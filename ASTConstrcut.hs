module ASTConstrcut(moduleConstruct) where

import Data.List
import ASTType
import qualified ParseType as P

pre :: [P.Decl]->P.Block
pre x = P.Block $ fmap P.DeclTerm x

moduleConstruct :: [Module]->Tag->[P.Decl] -> Module
moduleConstruct ms t ds = let (Block _ e _) = let   sym = concatMap (\(Module _ (ENV s _ _))->s) ms
                                                        in scopeConstructSameLevel [] sym t . pre $ ds in Module t e 

scopeConstructSameLevel :: [P.Decl]->SymTab->Tag->P.Block->Block
scopeConstructSameLevel param s tag (P.Block lt)= let   (def,term)=splitDef ([],[]) lt
                                                        symTab=(genSymTab tag param def )++ s -- ++preSymTab
                                                        typeTab=genTypeTab symTab def
                                                        decl=genDecl symTab def
                                                        env=ENV symTab decl typeTab
                                                        bs=concat $ zipWith (\x y->exprToBlock (tag++[x]) symTab y) tagCLtAny term
                                                            in Block bs env tag

----------------------------------------------------------------------------------------------------------------------

scopeConstruct :: [P.Decl]->SymTab->Tag->P.Block->Block
scopeConstruct param preSymTab tag (P.Block lt)= let    (def,term)=splitDef ([],[]) lt
                                                        symTab=(genSymTab tag param def )++preSymTab
                                                        typeTab=genTypeTab symTab def
                                                        decl=genDecl symTab def
                                                        env=ENV symTab decl typeTab
                                                        bs=concat $ zipWith (\x y->exprToBlock (tag++[x]) symTab y) tagCLtAny term
                                                            in Block bs env tag


splitDef (a,b) []=(reverse a,reverse b)
splitDef (xs,ys) (z:zs)=case z of 
                                (P.DeclTerm x)->splitDef (x:xs,ys) zs
                                (P.ExprTerm y)->splitDef (xs,y:ys) zs

genDecl :: SymTab->[P.Decl]->[Decl]
genDecl _ [] = []
genDecl s ((P.TypeDecl _ _):zs) = genDecl s zs 
genDecl s  ((P.DataStrDecl i ty ds):zs) = (ConstructOR (findTag s i) (countParam ty)):
                                            (map (\(P.TypeDecl t ty)->(ConstructOR (findTag s t) (countParam ty))) ds)
                                            ++(genDecl s zs)
genDecl s ((P.VarDecl i me mt):zs) = let    t=findTag s i
                                            me'=fmap (exprTransfer t s) me
                                            in case me' of
                                                (Just e)->(ValDecl t e):(genDecl s zs)
                                                _->error "Var not inited"
genDecl s ((P.FunDecl i is b):zs) = let t=findTag s i
                                        tsDecl=map (\x->P.VarDecl x Nothing Nothing) is 
                                        is'=map (\(x,y)->y) (genSymTab t [] tsDecl)
                                        b'=scopeConstruct tsDecl s t b
                                            in (FunDecl t is' b'):(genDecl s zs)

exprToBlock :: Tag->SymTab->P.EXPR->[Block]
exprToBlock t s (P.BlockExpr b mt) = [scopeConstruct [] s t b]
exprToBlock t s e = [BlockTerm . Term $ exprTransfer t s e]

countParam (P.SingleType _ _) = 0
countParam (P.ArrowType _ t _) = 1+(countParam t)

-------------------------------------------------------------------------------------------------------------------------------------------
getID :: P.Decl->[P.Id]
getID (P.FunDecl x _ _) = [x]
getID (P.VarDecl x _ _)=[x]
getID (P.DataStrDecl x _ decl)=x:(concat $ map getID decl)
getID (P.TypeDecl x _)=[x]

genSymTab :: Tag->[P.Decl]->[P.Decl]->SymTab
genSymTab tag prelt lt=let lt'=(concat (map getID prelt))++(map head . group . sort .concat $ map getID lt) in zipWith (\i t->(i,tagGen t tag)) lt' tagCLt

--------------------------------------------------------------------------------------------------------------------------------------------
genTypeTab :: SymTab->[P.Decl]->[TypeDecl]
genTypeTab s ps = concat (map (genType s) ps) 

genType :: SymTab->P.Decl->[TypeDecl]
genType  _ (P.FunDecl _ _ _) = []
genType symTab (P.VarDecl v e t) = case fmap (exprTransfer (findTag symTab v) symTab) e of 
                                        Nothing->[]
                                        Just expr->case getExprType expr of 
                                            Just ty->[TypeDecl (findTag symTab v) ty]
                                            Nothing->[]

                                    {-case t of 
                                    Nothing->[]
                                    Just ty->[TypeDecl (findTag symTab v) (typeTransfer symTab ty)]-}
genType symTab (P.DataStrDecl x t decl) = (TypeDecl (findTag symTab x) (typeTransfer symTab t)):
                                                    (concat $ map (genType symTab) decl)
genType symTab (P.TypeDecl x ty) = [TypeDecl (findTag symTab x) (typeTransfer symTab ty)]



getExprType (ConstExpr _ t) = t
getExprType (OpExpr _ _ t _) = t
getExprType (VarExpr _ t) = t
getExprType (ConExpr _ t _) = t
getExprType (BlockExpr _ t) = t
getExprType (LambdaExpr _ t) = t
getExprType (AppExpr _ t _) = t
getExprType (TypeExpr _) = Nothing

-------------------------------------------------------------------------------------------------------------------------------------------

typeTransfer :: SymTab -> P.TYPE -> Type
typeTransfer x e = typeTransferH x [0] e
typeTransferH :: SymTab->Tag->P.TYPE->Type --When clled,tag should always be [0] (meaning a type scope)
typeTransferH symTab tag a@(P.SingleType e tyc) = let   tyNamesp = zipWith (\x y->(y,tag++[x])) tagCLt (getTypeVar tyc)
                                                        symTab'=tyNamesp++symTab 
                                                        typeENV=TypeENV tag tyNamesp 
                                                            in case (exprTransfer (tag++[1]) symTab' e) of
                                                                VarExpr t _->SingleType t typeENV
                                                                a->ExprType a typeENV
typeTransferH symTab tag a@(P.ArrowType t1 t2 tyc) = let    tyNamesp=zipWith (\x y->(y,tag++[x])) tagCLt (getTypeVar tyc)
                                                            symTab'=tyNamesp++symTab
                                                            typeENV=TypeENV tag tyNamesp
                                                            t1'=typeTransferH symTab' (tag++[1]) t1
                                                            t2'=typeTransferH symTab' (tag++[2]) t2 
                                                                in ArrowType typeENV t1' t2'

-------------------------------------------------------------------------------------------------------------------------------------------



exprTransfer :: Tag->SymTab->P.EXPR->EXPR_C
exprTransfer tag symTab (P.ConstExpr v mt) = ConstExpr v (fmap (typeTransfer symTab) mt)
exprTransfer tag symTab (P.OpExpr (P.OP i) es mt) =  let    i'=findTag symTab i
                                                            t=fmap (typeTransfer symTab) mt
                                                            es'=zipWith (\t e->exprTransfer (tagGen t tag) symTab e) tagCLtAny es
                                                                in  OpExpr (OP i') es' t tag
exprTransfer tag symTab (P.VarExpr  i mt) = VarExpr (findTag symTab i) (fmap (typeTransfer symTab) mt)
exprTransfer tag symTab (P.BlockExpr b mt) = BlockExpr  (scopeConstruct [] symTab tag b) (fmap (typeTransfer symTab) mt)
exprTransfer tag symTab (P.AppExpr (P.FunApp i es) mt) = let    t=fmap (typeTransfer symTab) mt
                                                                i'=exprTransfer tag symTab i
                                                                es'=zipWith (\t e->exprTransfer (tagGen t tag) symTab e) tagCLtAny es
                                                                    in AppExpr (FunApp i' es') t tag
exprTransfer tag symTab (P.LambdaExpr (P.Lambda ids b) mt) = let    param=fmap (\x->P.VarDecl x Nothing  Nothing) ids 
                                                                    tags=genSymTab tag [] param
                                                                    t=fmap (typeTransfer symTab) mt
                                                                        in LambdaExpr (Lambda tags (scopeConstruct param symTab tag b)) t                            
exprTransfer tag symTab (P.ConExpr con mt) = let    t=fmap (typeTransfer symTab) mt
                                                    con'= case con of 
                                                            P.ConstrIf e1 e2 e3 mt2->(ConstrIf 
                                                                                            (exprTransfer (tag++[-1]) symTab e1) 
                                                                                            (exprTransfer (tag++[-2]) symTab e2) 
                                                                                            (exprTransfer (tag++[-3]) symTab e3) 
                                                                                            (fmap (typeTransfer symTab) mt2)) 
                                                            P.ConstrWhile e b mt2->(ConstrWhile
                                                                                            (exprTransfer (tag++[-1]) symTab e)
                                                                                            (scopeConstruct [] symTab (tag++[-2]) b)
                                                                                            (fmap (typeTransfer symTab) mt2))
                                                            P.ConReturn e mt2->(ConReturn (exprTransfer (tag++[-1]) symTab e) (fmap (typeTransfer symTab) mt2))
                                                            P.ConstrCase e pes mt2->(ConstrCase 
                                                                                            (exprTransfer (tag++[-1]) symTab e) 
                                                                                            (zipWith (pesTag symTab) (map (\x->tag++[x]) [-2,-3..]) pes)
                                                                                            (fmap (typeTransfer symTab) mt2))
                                                                in ConExpr con' t tag
exprTransfer tag symTab (P.TypeExpr ty) = TypeExpr (typeTransfer symTab ty)

pesTag :: SymTab->Tag->(P.Pattern,P.EXPR)->(Pattern,EXPR_C) --For now,we only accept pattern matching on constructors.
pesTag symTab tag ((P.Pattern ma e),eb) = let   a'=(fmap (\i->(i,tag++[1])) ma) 
                                                in  case e of
                                                    (P.VarExpr i _)->let    symTab'=(concat (fmap (:[]) a'))++[(i,vaTag)]++symTab
                                                                            vaTag=tag++[2]
                                                                                in (Pattern 
                                                                                        (fmap (\(_,y)->y) a') (VarExpr vaTag Nothing)
                                                                                    ,exprTransfer tag symTab' eb) 
                                                    (P.AppExpr (P.FunApp i es) _)->
                                                        let param=map (\(P.VarExpr i _)->i) es
                                                            symTab'=(concat (fmap (:[]) a'))++(zipWith (\x y->(x,tag++[y])) param [2,3..])++symTab
                                                            sym=(genSymTab tag [] (fmap (\x->P.VarDecl x Nothing Nothing) param))
                                                            es'=fmap (\(x,y)->y) sym
                                                            in    (Pattern 
                                                                        (fmap (\(_,y)->y) a') (exprTransfer tag symTab' e)
                                                                    ,exprTransfer tag symTab' eb)
                                                    _->error "Not supported pattern matching structure"

----------------------------------------------------------------------------------------------------------------------------------------------

getTypeVar :: [P.TypeConstraint]->[P.Id] --Used to get (forall a) out of type constraint
getTypeVar []= []
getTypeVar ((P.TypeConstraint (P.Id "Forall") [i]):xs)=i:getTypeVar xs -- Forall
getTypeVar x=error $ "Unsupport structure" ++ show x

findTag :: SymTab->P.Id->Tag
findTag ((iD,t):s) i=if iD==i then t else findTag s i 
findTag [] i=error ("Undefined varible"++(show i))
