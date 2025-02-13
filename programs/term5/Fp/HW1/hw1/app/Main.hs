module Main (main) where

import HW1.T3 

main :: IO ()
main = do
    let elements :: [Int]
        elements = [5, 3, 7, 2, 4, 6, 8]
    let tree = tFromList elements
    print (tree :: Tree Int)  
    print (tmember 4 tree)    
    print (tmember 10 tree)
