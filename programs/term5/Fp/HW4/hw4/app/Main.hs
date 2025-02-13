{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Main (main) where

import HW4.T2 (parseExpr)
import HW4.Types

-- Функция для тестирования парсера
testParser :: String -> String
testParser input =
  case parseExpr input of
    Success result -> "Parsed successfully: " ++ show result
    Error e        -> "Parse error: " ++ show e

-- Главная функция с набором тестов
main :: IO ()
main = do
  let tests = 
        [ -- Простые тесты
          ("42", "Parsed successfully: Val 42.0")
        , ("3 +4", "Parsed successfully: Op (Add (Val 3.0) (Val 4.0))")
        , ("(1 + 2) * 3", "Parsed successfully: Op (Mul (Op (Add (Val 1.0) (Val 2.0))) (Val 3.0))")
        , ("1 + 2 * (3 + 4)", "Parsed successfully: Op (Add (Val 1.0) (Op (Mul (Val 2.0) (Op (Add (Val 3.0) (Val 4.0))))))")
        , ("3 * (2 + 1)", "Parsed successfully: Op (Mul (Val 3.0) (Op (Add (Val 2.0) (Val 1.0))))")
        
        -- Тесты с делением
        , ("10 / 2", "Parsed successfully: Op (Div (Val 10.0) (Val 2.0))")
        , ("(10 / 2) * 3", "Parsed successfully: Op (Mul (Op (Div (Val 10.0) (Val 2.0))) (Val 3.0))")
        
        -- Тесты с ошибками
        , ("3 +", "Parse error: ErrorAtPos 3")
        , ("+ 3", "Parse error: ErrorAtPos 0")
        , ("3 /", "Parse error: ErrorAtPos 3")
        ]

  -- Запуск тестов
  mapM_ (\(input, expected) -> putStrLn $ "Test input: " ++ input ++ "\nExpected: " ++ expected ++ "\nResult: " ++ testParser input ++ "\n") tests


