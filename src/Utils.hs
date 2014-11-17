module Utils where

import Data.List.Split ( splitOn )

-- | The printf function
printf :: String -> [ String ] -> String 
printf str objs = printf' str "%" objs 


printf' :: String -> String -> [ String ] -> String 
printf' str del objs = concat $ zipWith (++) ( splitOn del str ) ( objs ++ repeat "" )

--splitOn :: Eq a => a -> [ a ] -> [[ a ]]
--splitOn x xs = splitOn' xs
--  where
--    splitOn' []         acc = acc
--    splitOn' ( x':xs' ) acc 
--      | x == x'    =   
--      | otherwise  =
