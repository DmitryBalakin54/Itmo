module Main (main) where
    
import HW2.T4

increment :: Fun Int
increment = F (+1)

double :: Fun Int
double = F (*2)

applyFun :: Fun Int -> Int -> Int
applyFun (F f) x = f x

main :: IO ()
main = do
  let f1 = increment
      f2 = double
      combinedFun = f1 <> f2  
  
  print $ applyFun combinedFun 3  
