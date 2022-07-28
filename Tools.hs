module Tools where

permutation [] = [[]]
permutation (x:xs) = let    lt=foldl (\zs@((x,xs,y:ys):_) _->(y,x:xs,ys):zs) [(x,[],xs)] xs
                                in concat $ map (\(a,x,y)->map (a:) (permutation (x++y))) lt