module Main where

import Parser 
import ASTConstrcut
import TypeCheck
import Codegen 
import Std 

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
    let ast = case parseRe of 
            Right x->moduleConstruct [stdM1,stdM2] [10] x 
            Left x->error $ show x
    let out=case typeCheck [stdM1,stdM2,ast] of
                Right e->error e
                Left _->std1 ++ std2 ++ modCGEN ast
    writeFile "a.out" out   


