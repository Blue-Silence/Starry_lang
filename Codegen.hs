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
                                        in lambdaCGEN param b 

                    

lambdaCGEN :: [Tag]->String->String --Currying
lambdaCGEN [] b = b
lambdaCGEN (t:ts) b = let   t'=tagCGEN t 
                            in "  (Lambda  " ++ "(" ++ t' ++ ")  " ++ lambdaCGEN ts b ++ "  )"



blockCGEN :: Block->String 
blockCGEN = undefined 









eCGEN :: EXPR_C->String
eCGEN = undefined














tagCGEN :: Tag->String
tagCGEN t = "  ___"++tagCGEN t++"  "
tagCGEN' [] = []
tagCGEN' [a] = show a
tagCGEN' (a:as) = show a ++ '-':(tagCGEN' as)