import Parser

import ParseType
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char

c1="{ var a=1 :: Int \n var b=a+1 \n foo(a b) \n }  "
c2="func foo(x y) ;;; { var a=1 :: Int ;; var b=a+1 ; foo(a b) ; return(a) ;};  "

c3="foo(a b)"

c4="var a=1 :: Int \n"
c5="a::Int"
c6="Hello \\n world"

c7="{ a::Int b::Int}"

c8="{ a::Int b :: Int}"

c9="{a::f() ;b :: Int;}"

c10="data fooT :: Type -> Type -> Type; {foo1 :: (a::x) -> (b::y) -> fooT(x y);}"

