{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import System.Console.GetOpt
import System.Environment (getArgs)
import Data.Grid (unGrid)
import Data.ListZipper (truncateLZ)
import HW6.T3 (Comonad19Grid, configure, simulate)

data SimulationOptions =
  SimulationOptions
    { soInfectionRate  :: Double
    , soIncubationDays :: Int
    , soIllnessDays    :: Int
    , soImmunityDays   :: Int
    , soGridDimension  :: Int
    , soStepCount      :: Int
    , soRandomSeed     :: Int
    }

defaultSimulationOptions :: SimulationOptions
defaultSimulationOptions = SimulationOptions 0.4 2 5 7 10 20 42

type Parser = String -> SimulationOptions -> SimulationOptions

optionParsers :: [(String, Parser, String)]
optionParsers =
  [ ("prob", \p o -> o {soInfectionRate = read p}, "Infection probability")
  , ("incub", \i o -> o {soIncubationDays = read i}, "Incubation period duration")
  , ("ill", \i o -> o {soIllnessDays = read i}, "Illness duration")
  , ("immun", \i o -> o {soImmunityDays = read i}, "Immunity duration")
  , ("grid-size", \s o -> o {soGridDimension = read s}, "Output grid size")
  , ("iterations", \n o -> o {soStepCount = read n}, "Number of simulation iterations")
  , ("seed", \s o -> o {soRandomSeed = read s}, "Initial random seed")
  ]

cliOptions :: [OptDescr (SimulationOptions -> SimulationOptions)]
cliOptions = 
  [ Option [] [name] (ReqArg parser "") help
  | (name, parser, help) <- optionParsers
  ]

parseCommandLine :: IO SimulationOptions
parseCommandLine = do
  args <- getArgs
  case getOpt Permute cliOptions args of
    (o, _, []) -> return $ foldl (flip id) defaultSimulationOptions o
    (_, _, errs) -> ioError (userError $ concat errs ++ usageInfo "Usage: comonad19 [OPTION...]" cliOptions)

data SimulationDisplay =
  SimulationDisplay
    { sdGrid :: Comonad19Grid
    , sdSize :: Int
    }

instance Show SimulationDisplay where
  show SimulationDisplay {..} = unlines $ map (concatMap show) truncated
    where
      truncated = truncateLZ sdSize $ fmap (truncateLZ sdSize) $ unGrid sdGrid

renderOutput :: [(Int, SimulationDisplay)] -> IO ()
renderOutput = mapM_ $ \(step, grid) -> do
  putStrLn $ "Step " ++ show step
  print grid
  putStrLn ""

main :: IO ()
main = do
  SimulationOptions {..} <- parseCommandLine
  case configure soInfectionRate soIncubationDays soIllnessDays soImmunityDays of
    Just config -> renderOutput simulationSteps
      where
        simulationSteps = zip [1..] $ map (`SimulationDisplay` soGridDimension) simulatedGrids
        simulatedGrids = take soStepCount $ simulate config soRandomSeed
    Nothing -> putStrLn "Incorrect args!"
