module Main where

import Control.DeepSeq
import Parser 
import ASTConstrcut
import TypeCheck
import Codegen 
import Std 

import ParseType(Id(..))
import ASTType(Module(..),ENV(..))

import System.IO
import System.Environment

import Text.Megaparsec


main = do 
    args <- getArgs
    let filename=case args of 
                    (a:_)->a 
                    _->error "Input file needed"
    h<-openFile filename ReadMode
    contents<-hGetContents h
    let parseRe = parse topMostParser "" contents
    putStrLn  $ seq parseRe "Stage 1" 
    let ast = case parseRe of 
            Right x->moduleConstruct [stdM1,stdM2] [10] x 
            Left x->error $ show x
    putStrLn $ seq ast "Stage 2"
    
    let mainTag=let (Module _ (ENV sym _ _)) = ast in lookup (Id "Main") sym

    let out=case typeCheck [stdM1,stdM2,ast] of
                Right e->error e
                Left _->std1 ++ std2 ++ modCGEN ast ++ case mainTag of 
                                                                Just mTag->"\n (define Main " ++ tagCGEN mTag ++ " )\n"
                                                                Nothing->""
    putStrLn $ deepseq out "Stage 3"
    writeFile "a.out" out   


