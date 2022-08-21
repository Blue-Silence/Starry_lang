module Codegen where

import ASTType

modCGEN :: Module->String
modCGEN (Module _ e) = envCGEN e

envCGEN :: ENV->String
envCGEN (ENV _ decls _) = concat . (map declCGEN) $ decls

declCGEN :: Decl->String
declCGEN (ValDecl tag e) = "  (define "
                            ++ (tagCGEN tag)
                            ++ "  "
                            ++ (eCGEN e)
                            ++ ") \n"
declCGEN (Param _) = ""
declCGEN (FunDecl ft pts b) = "  (define "
                              ++ (tagCGEN ft)
                              ++ "  "
                              ++ lambdaCGEN pts (blockCGEN b)
                              ++ "  ) \n"
declCGEN (ConstructOR tag pn) = let param=map (:[]) $ take pn [1,2..]
                                    b="(cons " ++ "  \"" ++ show tag ++ "\"  " ++ "#( " ++ (concat . (map tagCGEN)) param ++ "  ))"
                                        in  "  (define "
                                            ++ (tagCGEN tag)
                                            ++ "  "
                                            ++lambdaCGEN param b
                                            ++ ") \n"
declCGEN (Foreign _) = "" 
                                            



lambdaCGEN :: [Tag]->String->String --Currying
lambdaCGEN [] b = b
lambdaCGEN (t:ts) b = let   t'=tagCGEN t
                            in "  (Lambda  " ++ "(" ++ t' ++ ")  " ++ lambdaCGEN ts b ++ "  )"



blockCGEN :: Block->String
blockCGEN (BlockTerm (Term (ConExpr (ConReturn e _) _ _))) = "(set! __returnVal " ++ eCGEN e ++ "  )\n"
blockCGEN (BlockTerm (Term e)) = eCGEN e++" \n"
blockCGEN (Block bs env _) = "((Lambda () " ++ envCGEN env ++ "(define __returnVal 0)" ++ "\n" ++ (concat . (map blockCGEN)) bs ++ "__returnVal ))\n"









eCGEN :: EXPR_C->String
eCGEN (VarExpr t _) = tagCGEN t
eCGEN (BlockExpr b _) = blockCGEN b
eCGEN (LambdaExpr (Lambda symTab b) _) = "  (Lambda  (" ++ concat (map (\(x,y)->tagCGEN y) symTab) ++ ") " ++ blockCGEN b ++ ") \n  "
eCGEN (TypeExpr _) = ""
eCGEN (OpExpr (OP opt) es _ _) = "  (" ++ tagCGEN opt ++ concat (map eCGEN es) ++ ")  "
eCGEN (ConstExpr v _) = valCGEN v
eCGEN (AppExpr (FunApp e es) _ _) = let f=eCGEN e in foldl (\f e->" ( " ++ f ++ eCGEN e ++ " ) ") f es
eCGEN (ConExpr con _ _) = conStructCGEN con








tagCGEN :: Tag->String
tagCGEN t = "  ___VAR_"++tagCGEN' t++"  "
tagCGEN' [] = []
tagCGEN' [a] = show a
tagCGEN' (a:as) = show a ++ '-':(tagCGEN' as)


valCGEN :: Val->String
valCGEN (ConstStr str) = '\"' : str ++ "\""
valCGEN (ConstInt int) = show int
valCGEN (ConstChar c) = '\'' : show c ++ "\'"
valCGEN (ConstBool b) = if b then "#t" else "#f"

conStructCGEN :: ConStruct->String
conStructCGEN (ConReturn e _) = eCGEN e
conStructCGEN (ConstrIf eb et ef _) = "  (if " ++ eCGEN eb ++ eCGEN et ++ eCGEN ef ++ ")  "
conStructCGEN (ConstrWhile eb b _) = "  ((Lambda (x) (define (loop t)  (define val " ++ blockCGEN b ++ ") " ++ "(if " ++ eCGEN eb ++ " (loop t) " ++  "val ) )" ++ "(if " ++ eCGEN eb ++ " (loop t) " ++  "0 )) " ++ eCGEN eb ++ ")"
conStructCGEN (ConstrCase e pes _) = "(let ((caseVal "++ eCGEN e ++ ")) " ++ "(case " ++ "(car caseVal) " ++ concat (map patternCGEN pes) ++ ")  )\n"

patternCGEN (Pattern mt e,expr) = let   aDef=case mt of
                                            Nothing->""
                                            Just ta->"( " ++ tagCGEN ta ++ "caseVal" ++ ") "
                                                in case e of
                                                    (VarExpr t _)->
                                                            let bDef = "( " ++ tagCGEN t ++ "caseVal" ++ ") "
                                                                in " (else (let " ++ "("++ aDef ++ bDef ++ ")" ++ eCGEN expr ++ ")  )"
                                                    (AppExpr (FunApp (VarExpr f _) es) _ _)->
                                                            let fs='\"' : tagCGEN f ++ "\"  "
                                                                ltS=" (caseVLt (cdr caseVal)) "
                                                                bDef=concatMap (\(i,(VarExpr t _))->" (" ++ tagCGEN t ++ "(vector-ref caseVLt " ++ show i ++") )") (zip [0,1..] es)
                                                                    in " ( (" ++ fs ++ ") (let " ++ "("++ ltS ++ aDef ++ bDef ++ ")"++ eCGEN expr ++ ")  )"
                                                    _->error ""
